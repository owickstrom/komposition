{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
module FastCut.Render.FFmpeg
  ( extractFrameToFile
  , toRenderCommand
  , RenderResult(..)
  , renderComposition
  )
where

import           FastCut.Prelude
import qualified Prelude

import           Control.Lens
import qualified Data.List.NonEmpty            as NonEmpty
import qualified Data.Text                     as Text
import           Data.Vector                   (Vector)
import qualified Data.Vector                   as Vector
import           Pipes
import           System.Directory
import           System.FilePath
import qualified System.IO                     as IO
import           System.IO.Error               (isDoesNotExistError)
import           System.IO.Temp
import           System.Process
import           Text.Printf


import           FastCut.Duration
import           FastCut.Library
import           FastCut.MediaType
import           FastCut.Progress
import           FastCut.Render.Composition    (Composition (..))
import qualified FastCut.Render.Composition    as Composition
import           FastCut.Render.FFmpeg.Command (Command (Command))
import qualified FastCut.Render.FFmpeg.Command as Command
import           FastCut.Render.Timestamp

type FrameRate = Word

data Input mt where
  VideoAssetInput :: VideoAsset -> Input Video
  AudioAssetInput :: AudioAsset -> Input Audio
  StillFrameInput :: FilePath -> FrameRate -> Duration -> Input Video
  SilenceInput :: Duration -> Input Audio

deriving instance Eq (Input mt)

newtype InputIndex = InputIndex Integer

type PartStreamName = Text

data PartStream mt where
  VideoClipStream :: InputIndex -> TimeSpan -> PartStream Video
  AudioClipStream :: InputIndex -> TimeSpan -> PartStream Audio
  StillFrameStream :: InputIndex -> PartStream Video
  SilenceStream :: InputIndex -> PartStream Audio

data CommandInput mt = CommandInput
  { inputs       :: NonEmpty (Input mt)
  , inputStreams :: NonEmpty (PartStreamName, PartStream mt)
  }

extractFrameToFile
  :: FrameRate -> Composition.StillFrameMode -> Asset Video -> TimeSpan -> FilePath -> IO FilePath
extractFrameToFile frameRate mode videoAsset ts frameDir = do
  let sourcePath = videoAsset ^. assetMetadata . path
      -- Not the best hash...
      frameHash =
        hash
          ( mode
          , sourcePath
          , durationToSeconds (spanStart ts)
          , durationToSeconds (spanEnd ts))
      frameFilePath = frameDir </> show (abs frameHash) <> ".png"
  removeFile frameFilePath `catch` \case
    e | isDoesNotExistError e -> return ()
      | otherwise -> throwIO e
  case mode of
    Composition.FirstFrame -> extractFrame sourcePath (spanStart ts) frameFilePath
    Composition.LastFrame ->
      let frameDuration = durationFromSeconds (1 / fromIntegral frameRate)
      in extractFrame sourcePath (spanEnd ts - frameDuration) frameFilePath
  return frameFilePath
  where
    extractFrame :: FilePath -> Duration -> FilePath -> IO ()
    extractFrame sourcePath startAfter frameFileName = do
      putStrLn
        ("Extracting frame at " <> printTimestamp startAfter <> " from " <>
         toS sourcePath <>
         ".")
      let allArgs =
            [ "-nostdin"
            , "-ss"
            , printf "%f" (durationToSeconds startAfter)
            , "-i"
            , sourcePath
            , "-t"
            , "1"
            , "-vframes"
            , "1"
            , frameFileName
            ]
      putStrLn $ Text.unwords ("ffmpeg" : map toS allArgs)
      runFFmpeg $ proc "ffmpeg" allArgs
    runFFmpeg cmd = do
      (exit, _, err) <- readCreateProcessWithExitCode cmd ""
      when
        (exit /= ExitSuccess)
        (Prelude.fail ("Couldn't extract frame from video file: " <> err))

toCommandInput
  :: FilePath
  -> FrameRate
  -> Int
  -> NonEmpty (Composition.CompositionPart mt)
  -> IO (CommandInput mt)
toCommandInput tmpDir frameRate startAt parts' =
  parts'
  & NonEmpty.zip (startAt :| [succ startAt ..])
  & traverse toPartStream
  & (`runStateT` mempty)
  & (>>= toCommandInputOrPanic)
  where
    toInputIndex i = InputIndex (fromIntegral (i + startAt))
    addUniqueInput :: Eq (Input mt) =>
         Input mt -> StateT (Vector (Input mt)) IO InputIndex
    addUniqueInput input = do
      inputs <- get
      case Vector.findIndex (== input) inputs of
        Just idx -> return (toInputIndex (fromIntegral idx))
        Nothing -> do
          put (inputs `Vector.snoc` input)
          return (toInputIndex (Vector.length inputs))
    toPartStream ::
         (Int, Composition.CompositionPart mt)
      -> StateT (Vector (Input mt)) IO (PartStreamName, PartStream mt)
    toPartStream =
      \case
        (vi, Composition.VideoClip asset ts) -> do
          ii <- addUniqueInput (VideoAssetInput asset)
          return ("v" <> show vi, VideoClipStream ii ts)
        (vi, Composition.StillFrame mode asset ts duration') -> do
          frameFile <- lift (extractFrameToFile frameRate mode asset ts tmpDir)
          ii <- addUniqueInput (StillFrameInput frameFile frameRate duration')
          return ("v" <> show vi, StillFrameStream ii)
        (ai, Composition.AudioClip asset) -> do
          ii <- addUniqueInput (AudioAssetInput asset)
          return
            ("a" <> show ai, AudioClipStream ii (TimeSpan 0 (durationOf asset)))
        (ai, Composition.Silence duration') -> do
          ii <- addUniqueInput (SilenceInput duration')
          return ("a" <> show ai, SilenceStream ii)
    toCommandInputOrPanic (streams, inputs) = do
      inputs' <-
        maybe
          (panic "No inputs found for FFmpeg command.")
          pure
          (NonEmpty.nonEmpty (Vector.toList inputs))
      pure (CommandInput inputs' streams)

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
  -> CommandInput Video
  -> CommandInput Audio
  -> Command
toRenderCommand outFile videoInput audioInput =
  Command
  { output = outFile
  , inputs =
      NonEmpty.map toVideoInput (inputs videoInput) <>
      NonEmpty.map toAudioInput (inputs audioInput)
  , filterGraph =
      Command.FilterGraph
        (videoInputChains <> audioInputChains <> (videoChain :| [audioChain]))
  , mappings = [videoStream, audioStream]
  , format = "mp4"
  }
  where
    namedStream n =
      Command.StreamSelector (Command.StreamName n) Nothing Nothing
    videoStream = namedStream "video"
    audioStream = namedStream "audio"
    videoChain = Command.FilterChain (videoConcat :| [videoSetStart])
    audioChain = Command.FilterChain (audioConcat :| [audioSetStart])
    videoInputChains = map toVideoInputChain (inputStreams videoInput)
    audioInputChains = map toAudioInputChain (inputStreams audioInput)
    videoConcat =
      Command.RoutedFilter
        (NonEmpty.toList
           (map (toStreamNameOnlySelector . fst) (inputStreams videoInput)))
        (Command.Concat (fromIntegral (length (inputStreams videoInput))) 1 0)
        []
    audioConcat =
      Command.RoutedFilter
        (NonEmpty.toList
           (map (toStreamNameOnlySelector . fst) (inputStreams audioInput)))
        (Command.Concat (fromIntegral (length (inputStreams audioInput))) 0 1)
        []
    videoSetStart = Command.RoutedFilter [] Command.SetPTSStart [videoStream]
    audioSetStart =
      Command.RoutedFilter [] Command.AudioSetPTSStart [audioStream]
    --
    -- Conversion helpers:
    --
    toVideoInput :: Input Video -> Command.Source
    toVideoInput =
      \case
        VideoAssetInput asset ->
          Command.FileSource (asset ^. assetMetadata . path)
        StillFrameInput frameFile frameRate duration' ->
          Command.StillFrameSource frameFile frameRate duration'
    toAudioInput :: Input Audio -> Command.Source
    toAudioInput =
      \case
        AudioAssetInput asset ->
          Command.FileSource (asset ^. assetMetadata . path)
        SilenceInput duration' -> Command.AudioNullSource duration'
    toStreamNameOnlySelector streamName =
      Command.StreamSelector (Command.StreamName streamName) Nothing Nothing
    toVideoInputChain ::
         (PartStreamName, PartStream Video) -> Command.FilterChain
    toVideoInputChain (streamName, partStream) =
      case partStream of
        VideoClipStream ii ts ->
          trimmedIndexedInput Command.Video streamName ii ts
        StillFrameStream i ->
          Command.FilterChain
            (Command.RoutedFilter
               [indexedStreamSelector Command.Video i]
               Command.SetPTSStart
               [ Command.StreamSelector
                   (Command.StreamName streamName)
                   Nothing
                   Nothing
               ] :|
             [])
    toAudioInputChain ::
         (PartStreamName, PartStream Audio) -> Command.FilterChain
    toAudioInputChain (streamName, partStream) =
      case partStream of
        AudioClipStream ii ts ->
          trimmedIndexedInput Command.Audio streamName ii ts
        SilenceStream i ->
          Command.FilterChain
            (Command.RoutedFilter
               [indexedStreamSelector Command.Audio i]
               Command.AudioSetPTSStart
               [ Command.StreamSelector
                   (Command.StreamName streamName)
                   Nothing
                   Nothing
               ] :|
             [])
    indexedStreamSelector track (InputIndex i) =
      Command.StreamSelector (Command.StreamIndex i) (Just track) (Just 0)
    trimmedIndexedInput track streamName (InputIndex i) ts =
      Command.FilterChain
        (Command.RoutedFilter
           [ Command.StreamSelector
               (Command.StreamIndex i)
               (Just track)
               (Just 0)
           ]
           (Command.Trim (spanStart ts) (durationOf ts))
           [] :|
         [ Command.RoutedFilter
             []
             Command.SetPTSStart
             [ Command.StreamSelector
                 (Command.StreamName streamName)
                 Nothing
                 Nothing
             ]
         ])

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
  videoInput <- lift (toCommandInput tmpDir frameRate 0 video)
  audioInput <- lift (toCommandInput tmpDir frameRate (length (inputs videoInput)) audio)

  let renderCmd = toRenderCommand outFile videoInput audioInput
      allArgs   = "-v" : "quiet" : "-stats" : "-nostdin" : map
        toS
        (Command.printCommandLineArgs renderCmd)
      process = proc "ffmpeg" allArgs
  lift (putStrLn (Prelude.unwords ("ffmpeg" : allArgs)))
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
