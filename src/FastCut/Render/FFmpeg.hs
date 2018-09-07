{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
module FastCut.Render.FFmpeg
  ( toRenderCommand
  , RenderResult(..)
  , renderComposition
  )
where

import           FastCut.Prelude
import qualified Prelude

import           Control.Lens
import qualified Data.List.NonEmpty            as NonEmpty
import qualified Data.Text                     as Text
import           Pipes
import           System.FilePath
import qualified System.IO                     as IO
import           System.IO.Temp
import           System.Process

import           FastCut.Duration
import           FastCut.Library
import           FastCut.MediaType
import           FastCut.Progress
import           FastCut.Render.Composition    (Composition (..))
import qualified FastCut.Render.Composition    as Composition
import           FastCut.Render.FFmpeg.Command (Command (..))
import qualified FastCut.Render.FFmpeg.Command as Command
import           FastCut.Render.Timestamp

type FrameRate = Word

data Part mt where
  Clip :: Asset mt -> Part mt
  StillFrame :: FilePath -> FrameRate -> Duration -> Part Video
  Silence :: Duration -> Part Audio

type IndexedPart mt = (Int, Part mt)

extractNumberOfFrames :: FilePath -> IO Integer
extractNumberOfFrames videoFile = do
  let probeCommand = proc "ffprobe" ["-show_streams", videoFile]
  (exit, sout, _) <- readCreateProcessWithExitCode probeCommand ""
  when
    (exit /= ExitSuccess)
    (Prelude.fail
      ("Couldn't read number of frames in video file: " <> videoFile)
    )
  Text.pack sout
    & Text.lines
    & mapMaybe parseKeyValue
    & find ((== "nb_frames") . fst)
    & map snd
    & (>>= readDecimal)
    & maybe
        (Prelude.fail "Couldn't parse number of frames from ffprobe output")
        return
  where
    parseKeyValue t = case Text.splitOn "=" t of
      [key, value] -> Just (key, value)
      _            -> Nothing


extractFrameToFile
  :: Composition.StillFrameMode -> Asset Video -> FilePath -> IO ()
extractFrameToFile mode (VideoAsset meta) frameFile = do
  let sourcePath = meta ^. path
  frameIndex <- case mode of
    Composition.FirstFrame -> return 0
    Composition.LastFrame  -> pred <$> extractNumberOfFrames sourcePath
  putStrLn
    (  "Extracting frame "
    <> show frameIndex
    <> " of file "
    <> sourcePath
    <> " to "
    <> frameFile
    )
  let ffmpegCommand = proc
        "ffmpeg"
        [ "-nostdin"
        , "-i"
        , sourcePath
        , "-vf"
        , "select='eq(n," <> show frameIndex <> ")'"
        , "-vframes"
        , "1"
        , frameFile
        ]
  (exit, _, err) <- readCreateProcessWithExitCode ffmpegCommand ""
  when
    (exit /= ExitSuccess)
    (Prelude.fail
      ("Couldn't extract frame from video file (" <> sourcePath <> "): " <> err)
    )

toIndexedParts
  :: FilePath
  -> FrameRate
  -> Int
  -> NonEmpty (Composition.CompositionPart mt)
  -> IO (NonEmpty (IndexedPart mt))
toIndexedParts tmpDir frameRate startAt = traverse toPart
  . NonEmpty.zip (startAt :| [succ startAt ..])
  where
    toPart :: (Int, Composition.CompositionPart mt) -> IO (IndexedPart mt)
    toPart = \case
      (i, Composition.Clip asset                     ) -> return (i, Clip asset)
      (i, Composition.StillFrame mode asset duration') -> do
        let frameFile = tmpDir </> show i <> ".png"
        extractFrameToFile mode asset frameFile
        return (i, StillFrame frameFile frameRate duration')
      (i, Composition.Silence duration') -> return (i, Silence duration')

parseTimestampFromProgress :: Text -> Maybe Duration
parseTimestampFromProgress line = parseTimestamp
  =<< Prelude.lookup "time" (toPairs (splitByWhitespaceOrEquals line))
  where
    splitByWhitespaceOrEquals =
      filter (not . Text.null) . Text.split (`elem` ['\t', ' ', '='])
    toPairs (key : value : rest) = (key, value) : toPairs rest
    toPairs _                    = []

fromCarriageReturnSplit :: Handle -> Producer Text IO ()
fromCarriageReturnSplit h = go mempty
  where
    go buf = lift (IO.hIsEOF h) >>= \case
      True  -> yield buf
      False -> do
        c <- lift (IO.hGetChar h)
        if c == '\r'
          then yield buf >> go mempty
          else go (buf <> Text.singleton c)

toRenderCommand
  :: FilePath
  -> NonEmpty (IndexedPart Video)
  -> NonEmpty (IndexedPart Audio)
  -> Command
toRenderCommand outFile videoParts audioParts = Command {output = outFile, ..}
  where
    inputs =
      NonEmpty.map toVideoInput videoParts
        <> NonEmpty.map toAudioInput audioParts
    videoStream =
      Command.StreamSelector (Command.StreamName "video") Nothing Nothing
    audioStream =
      Command.StreamSelector (Command.StreamName "audio") Nothing Nothing
    filterGraph = Command.FilterGraph (videoChain :| [audioChain])
    videoChain  = Command.FilterChain (videoConcat :| [videoSetStart])
    audioChain  = Command.FilterChain (audioConcat :| [audioSetStart])
    videoConcat = Command.RoutedFilter
      (NonEmpty.toList (map (toConcatInput Command.Video) videoParts))
      (Command.Concat (fromIntegral (length videoParts)) 1 0)
      []
    audioConcat = Command.RoutedFilter
      (NonEmpty.toList (map (toConcatInput Command.Audio) audioParts))
      (Command.Concat (fromIntegral (length audioParts)) 0 1)
      []
    videoSetStart = Command.RoutedFilter [] Command.SetPTSStart [videoStream]
    audioSetStart =
      Command.RoutedFilter [] Command.AudioSetPTSStart [audioStream]
    mappings = [videoStream, audioStream]
    format   = "mp4"
    --
    -- Conversion helpers:
    --
    toVideoInput :: IndexedPart Video -> Command.Source
    toVideoInput = \case
      (_, Clip (VideoAsset asset)) -> Command.FileSource (asset ^. path)
      (_, StillFrame imagePath frameRate duration') ->
        Command.StillFrameSource imagePath frameRate duration'
    toAudioInput :: IndexedPart Audio -> Command.Source
    toAudioInput = \case
      (_, Clip (AudioAsset asset)) -> Command.FileSource (asset ^. path)
      (_, Silence duration'      ) -> Command.AudioNullSource duration'
    toConcatInput track (i, _) = Command.StreamSelector
      (Command.StreamIndex (fromIntegral i))
      (Just track)
      (Just 0)

data RenderResult
  = Success
  | ProcessFailed Text

renderComposition
  :: FrameRate
  -> FilePath
  -> Composition
  -> Producer ProgressUpdate IO RenderResult
renderComposition frameRate outFile c@(Composition video audio) = do
  canonical     <- lift getCanonicalTemporaryDirectory
  tmpDir        <- lift (createTempDirectory canonical "fastcut.render")
  indexedVideos <- lift (toIndexedParts tmpDir frameRate 0 video)
  indexedAudios <- lift
    (toIndexedParts tmpDir frameRate (length indexedVideos) audio)

  let renderCmd = toRenderCommand outFile indexedVideos indexedAudios
      allArgs   = "-v" : "quiet" : "-stats" : "-nostdin" : map
        toS
        (Command.printCommandLineArgs renderCmd)
      process = proc "ffmpeg" allArgs
  -- lift (putStrLn (Prelude.unwords ("ffmpeg" : allArgs)))
  (_, _, Just progressOut, ph) <- lift
    (createProcess_ "" process { std_err = CreatePipe })
  lift (IO.hSetBuffering progressOut IO.NoBuffering)
  let totalDuration = durationToSeconds (durationOf c)
  Pipes.for (fromCarriageReturnSplit progressOut) $ \line -> do
    lift (putStrLn line)
    case parseTimestampFromProgress line of
      Just currentDuration -> yield
        (ProgressUpdate (durationToSeconds currentDuration / totalDuration))
      Nothing -> return ()
  lift (waitForProcess ph) >>= \case
    ExitSuccess   -> return Success
    ExitFailure e -> return
      (ProcessFailed ("FFmpeg command failed with exit code: " <> show e))
