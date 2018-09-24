{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module FastCut.Import.Audio where

import           FastCut.Prelude     hiding (catch)
import qualified Prelude

import qualified Codec.FFmpeg.Probe  as Probe
import           Control.Monad.Catch
import qualified Data.Char           as Char
import qualified Data.Text           as Text
import           Data.Time.Clock
import           Pipes
import           Pipes               (Producer)
import           Pipes.Safe
import           System.Directory
import           System.FilePath
import qualified System.IO           as IO
import           System.IO.Temp
import           System.Process

import           FastCut.Duration
import           FastCut.Library
import           FastCut.MediaType
import           FastCut.Progress

data AudioImportError
  = UnexpectedError FilePath Text
  | ProcessFailed Text Int (Maybe Text)
  | CouldNotReadMaximumAmplitude FilePath
  | TranscodingFailed Text
  deriving (Show, Eq)

instance Exception AudioImportError

fromCarriageReturnOrNewlineSplit :: MonadIO m => Handle -> Producer Text m ()
fromCarriageReturnOrNewlineSplit h = go mempty
  where
    go buf = liftIO (IO.hIsEOF h) >>= \case
      True  -> yield buf
      False -> do
        c <- liftIO (IO.hGetChar h)
        if c `elem` ['\r', '\n']
          then yield buf >> go mempty
          else go (buf <> Text.singleton c)

runSoxWithProgress
  :: (MonadIO m, MonadSafe m)
  => (Double -> ProgressUpdate)
  -> [Prelude.String]
  -> Producer ProgressUpdate m ()
runSoxWithProgress toProgress args = do
  (_, _, Just progressOut, ph) <- liftIO
    (createProcess_ "" (proc "sox" ("-S" : args)) { std_err = CreatePipe })
  liftIO (IO.hSetBuffering progressOut IO.NoBuffering)
  fromCarriageReturnOrNewlineSplit progressOut >-> yieldProgress
  liftIO (waitForProcess ph) >>= \case
    ExitSuccess   -> return ()
    ExitFailure e -> throwIO (ProcessFailed "sox" e Nothing)
  where
    yieldProgress :: MonadIO m => Pipe Text ProgressUpdate m ()
    yieldProgress = forever $ do
      line <- await
      case Text.splitOn ":" (Text.takeWhile (not . Char.isSpace) line) of
        ["In", percentStr] ->
          case readDouble (Text.init percentStr) of
            Just d  -> yield (toProgress (d / 100))
            Nothing -> return ()
        _ -> return ()

normalizeAudio
  :: (MonadIO m, MonadSafe m)
  => FilePath -- Temporary directory to save normalized file in
  -> FilePath -- Source path
  -> Producer ProgressUpdate m FilePath -- Action with progress updates, returning the normalized file path
normalizeAudio tempDir sourcePath = do
  let outPath = tempDir </> "normalized.wav"
  runSoxWithProgress (ProgressUpdate "Normalizing audio") ["--norm", sourcePath, outPath]
  return outPath

splitAudioBySilence
  :: (MonadIO m, MonadSafe m)
  => FilePath
  -> FilePath
  -> FilePath
  -> Producer ProgressUpdate m [FilePath]
splitAudioBySilence outputDir fileNameTemplate sourcePath = do
  liftIO (createDirectoryIfMissing True outputDir)
  runSoxWithProgress
    (ProgressUpdate "Splitting by silence")
    [ sourcePath
    , outputDir </> fileNameTemplate
    , "silence"
    , "1", "0",   "0.05%"
    , "1", "0.5", "0.05%"
    , ":", "newfile"
    , ":", "restart"
    ]
  relFiles <- liftIO (System.Directory.listDirectory outputDir)
  return (map (outputDir </>) (sort relFiles))

dropSilentChunks
  :: (MonadIO m, MonadSafe m)
  => [FilePath]
  -> Producer ProgressUpdate m [FilePath]
dropSilentChunks fs =
  fold <$> zipWithM go [(1::Int)..] fs
  where
    count = length fs
    go n audioFilePath = do
      yield (ProgressUpdate "Dropping silent chunks" (fromIntegral n / fromIntegral count))
      lift (getAudioFileMaxAmplitude audioFilePath) >>= \case
        d | d > 0.05 -> return [audioFilePath]
          | otherwise -> liftIO (removeFile audioFilePath) >> return []

importAudioFile ::
     (MonadIO m, MonadMask m, MonadSafe m)
  => FilePath
  -> FilePath
  -> Producer ProgressUpdate m (Asset Audio)
importAudioFile audioFile outDir = do
  Pipes.yield (ProgressUpdate "Importing Audio" 0)
  -- Copy asset to working directory
  assetPath <-
    liftIO $ do
      createDirectoryIfMissing True outDir
      let assetPath = outDir </> takeFileName audioFile
      copyFile audioFile assetPath
      return assetPath
  -- Generate thumbnail and return asset
  Pipes.yield (ProgressUpdate "Importing Audio" 0.5) *>
    (filePathToAudioAsset outDir assetPath) <*
    Pipes.yield (ProgressUpdate "Importing Audio" 1)

importAudioFileAutoSplit ::
     (MonadIO m, MonadMask m, MonadSafe m)
  => FilePath
  -> FilePath
  -> Producer ProgressUpdate m [Asset Audio]
importAudioFileAutoSplit audioFile outDir = do
  liftIO (createDirectoryIfMissing True outDir)
  -- TODO: bracket to make sure temp directory is deleted
  tempDir <- liftIO $ do
    canonical <- getCanonicalTemporaryDirectory
    createTempDirectory canonical "fastcut.audio.import"
  -- TODO: use file md5 digest in filename (or for a subdirectory) to avoid collisions
  chunks <-
    divideProgress4
      (lift (transcodeAudioFileToWav tempDir audioFile))
      (normalizeAudio tempDir)
      (splitAudioBySilence (outDir </> "audio-chunks") (takeBaseName audioFile <> "-%5n.wav"))
      dropSilentChunks
  lift (mapM (filePathToAudioAsset outDir) chunks)

isSupportedAudioFile :: FilePath -> Bool
isSupportedAudioFile p = takeExtension p `elem` [".wav", ".mp3", ".m4a", ".aiff", ".aac"]

filePathToAudioAsset ::
     (MonadMask m, MonadIO m)
  => FilePath
  -> FilePath
  -> m (Asset Audio)
filePathToAudioAsset _outDir audioFilePath = do
  d <- getAudioFileDuration audioFilePath
  let meta = AssetMetadata (OriginalPath audioFilePath) d
  -- TODO: Generate waveform thumbnail
  return (AudioAsset meta)

getAudioFileDuration :: (MonadMask m, MonadIO m) => FilePath -> m Duration
getAudioFileDuration f =
  Duration . picosecondsToDiffTime . (* 1000000) . fromIntegral <$>
  Probe.withAvFile f Probe.duration

getAudioFileMaxAmplitude :: MonadIO m => FilePath -> m Double
getAudioFileMaxAmplitude inPath = do
  (ex, _, err) <- liftIO (readCreateProcessWithExitCode (proc "sox" [inPath, "-n", "stat"]) "")
  case ex of
    ExitSuccess ->
      let parts = map (map Text.strip . Text.splitOn ":") (Text.lines (toS err))
      in maybe (throwIO (CouldNotReadMaximumAmplitude inPath)) return $ do
         (_ : ampStr : _) <- find ((==) (Just "Maximum amplitude") . headMay) parts
         readDouble ampStr
    ExitFailure c -> throwIO (ProcessFailed "sox" c (Just (toS err)))

transcodeAudioFileToWav :: MonadIO m => FilePath -> FilePath -> m FilePath
transcodeAudioFileToWav tempDir inPath = do
  -- TODO: use md5 digest to avoid collisions
  let outPath = tempDir </> takeBaseName inPath <> ".wav"
  (ex, _, err) <- liftIO (readCreateProcessWithExitCode (proc "ffmpeg" ["-i", inPath, "-f", "wav", outPath]) "")
  case ex of
    ExitSuccess   -> return outPath
    ExitFailure _ -> throwIO (TranscodingFailed (toS err))
