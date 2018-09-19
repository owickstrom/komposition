{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module FastCut.Import.Audio where

import           FastCut.Prelude     hiding (catch)

import qualified Codec.FFmpeg.Probe  as Probe
import           Control.Monad.Catch
import           Data.Time.Clock
import           Pipes               (Producer)
import qualified Pipes
import           System.Directory
import           System.FilePath

import           FastCut.Duration
import           FastCut.Library
import           FastCut.MediaType
import           FastCut.Progress

data AudioImportError
  = UnexpectedError FilePath Text
  deriving (Show, Eq)

importAudioFile ::
     (MonadIO m, MonadMask m)
  => FilePath
  -> FilePath
  -> Producer ProgressUpdate m (Either AudioImportError (Asset Audio))
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
    (lift (filePathToAudioAsset outDir assetPath & runExceptT)) <*
    Pipes.yield (ProgressUpdate "Importing Audio" 1)

isSupportedAudioFile :: FilePath -> Bool
isSupportedAudioFile p = takeExtension p `elem` [".wav", ".mp3", ".m4a", ".aiff", ".aac"]

filePathToAudioAsset ::
     (MonadError AudioImportError m, MonadMask m, MonadIO m)
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
