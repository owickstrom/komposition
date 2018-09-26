{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
module FastCut.Render.FFmpeg
  ( Source(..)
  , extractFrameToFile
  , toRenderCommand
  , RenderError(..)
  , renderComposition
  )
where

import           FastCut.Prelude
import qualified Prelude

import           Control.Lens
import qualified Data.List.NonEmpty         as NonEmpty
import qualified Data.Text                  as Text
import           Data.Vector                (Vector)
import qualified Data.Vector                as Vector
import           Pipes                      (Producer)
import           Pipes.Safe                 (MonadSafe)
import           System.Directory
import           System.FilePath
import           System.IO.Temp
import           System.Process
import           Text.Printf

import           FastCut.Duration
import           FastCut.FFmpeg.Command     (Command (Command))
import qualified FastCut.FFmpeg.Command     as Command
import           FastCut.FFmpeg.Process
import           FastCut.Library
import           FastCut.MediaType
import           FastCut.Progress
import           FastCut.Render.Composition (Composition (..))
import qualified FastCut.Render.Composition as Composition
import           FastCut.Timestamp
import           FastCut.VideoSettings

data Source (mt :: MediaType) where
  VideoOriginal :: Source Video
  VideoProxy :: Source Video
  AudioOriginal :: Source Audio

instance Hashable (Source mt) where
  hashWithSalt s = \case
    VideoOriginal -> s
    VideoProxy    -> s + 1
    AudioOriginal -> s + 2

data Input mt where
  VideoAssetInput :: OriginalPath -> Input Video
  AudioAssetInput :: OriginalPath -> Input Audio
  StillFrameInput :: FilePath -> Duration -> Input Video
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

extractFrameToFile ::
     VideoSettings
  -> Composition.StillFrameMode
  -> Source Video
  -> Asset Video
  -> TimeSpan
  -> FilePath
  -> IO FilePath
extractFrameToFile videoSettings mode videoSource videoAsset ts frameDir = do
  let sourcePath =
        case videoSource of
          VideoOriginal -> videoAsset ^. assetMetadata . path . unOriginalPath
          VideoProxy -> videoAsset ^. videoAssetProxy . unProxyPath
      -- Not the best hash...
      frameHash =
        hash
          ( mode
          , videoSource
          , sourcePath
          , videoSettings ^. resolution
          , durationToSeconds (spanStart ts)
          , durationToSeconds (spanEnd ts))
      frameFilePath = frameDir </> show (abs frameHash) <> ".png"
  case mode of
    Composition.FirstFrame ->
      extractFrame sourcePath (spanStart ts) frameFilePath
    Composition.LastFrame ->
      let frameDuration =
            durationFromSeconds (1 / fromIntegral (videoSettings ^. frameRate))
      in extractFrame sourcePath (spanEnd ts - frameDuration) frameFilePath
  return frameFilePath
  where
    extractFrame :: FilePath -> Duration -> FilePath -> IO ()
    extractFrame sourcePath startAfter frameFileName =
      unlessM (doesFileExist frameFileName) $ do
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
              , "-vf"
              , "scale=" <> show (videoSettings ^. resolution . width) <> ":" <> show (videoSettings ^. resolution . height)
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
  :: SMediaType mt
  -> FilePath
  -> VideoSettings
  -> Source mt
  -> Int
  -> NonEmpty (Composition.CompositionPart mt)
  -> IO (CommandInput mt)
toCommandInput mediaType tmpDir videoSettings source startAt parts' =
  parts' & NonEmpty.zip (startAt :| [succ startAt ..]) & traverse toPartStream &
  (`runStateT` mempty) &
  (>>= toCommandInputOrPanic)
  where
    toInputIndex i = InputIndex (fromIntegral (i + startAt))
    addUniqueInput ::
         Eq (Input mt) => Input mt -> StateT (Vector (Input mt)) IO InputIndex
    addUniqueInput input = do
      inputs <- get
      case Vector.findIndex (== input) inputs of
        Just idx -> return (toInputIndex (fromIntegral idx))
        Nothing -> do
          put (inputs `Vector.snoc` input)
          return (toInputIndex (Vector.length inputs))
    toPartStream (i, p) =
      case (mediaType, i, p) of
        (SVideo, vi, Composition.VideoClip asset ts) -> do
          ii <- addUniqueInput (VideoAssetInput (asset ^. assetMetadata . path))
          return ("v" <> show vi, VideoClipStream ii ts)
        (SVideo, vi, Composition.StillFrame mode asset ts duration') -> do
          frameFile <-
            lift (extractFrameToFile videoSettings mode source asset ts tmpDir)
          ii <- addUniqueInput (StillFrameInput frameFile duration')
          return ("v" <> show vi, StillFrameStream ii)
        (SAudio, ai, Composition.AudioClip asset) -> do
          ii <- addUniqueInput (AudioAssetInput (asset ^. assetMetadata . path))
          return
            ("a" <> show ai, AudioClipStream ii (TimeSpan 0 (durationOf asset)))
        (SAudio, ai, Composition.Silence duration') -> do
          ii <- addUniqueInput (SilenceInput duration')
          return ("a" <> show ai, SilenceStream ii)
    toCommandInputOrPanic (streams, inputs) = do
      inputs' <-
        maybe
          (panic "No inputs found for FFmpeg command.")
          pure
          (NonEmpty.nonEmpty (Vector.toList inputs))
      pure (CommandInput inputs' streams)

toRenderCommand
  :: VideoSettings
  -> Command.Output
  -> CommandInput Video
  -> CommandInput Audio
  -> Command
toRenderCommand videoSettings output videoInput audioInput =
  Command
  { output = output
  , inputs =
      NonEmpty.map toVideoInput (inputs videoInput) <>
      NonEmpty.map toAudioInput (inputs audioInput)
  , filterGraph =
      Just . Command.FilterGraph $
        videoInputChains <> audioInputChains <> (videoChain :| [audioChain])
  , mappings = [videoStream, audioStream]
  , format =
      case output of
        Command.HttpStreamingOutput{} -> Just "matroska"
        Command.UdpStreamingOutput{}  -> Just "matroska"
        Command.FileOutput{}          -> Just "mp4"
  , vcodec = Just "h264"
  , acodec = Just "aac"
  , frameRate = Just (videoSettings ^. frameRate)
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
        VideoAssetInput p -> Command.FileSource (p ^. unOriginalPath)
        StillFrameInput frameFile duration' ->
          Command.StillFrameSource
            frameFile
            (videoSettings ^. frameRate)
            duration'
    toAudioInput :: Input Audio -> Command.Source
    toAudioInput =
      \case
        AudioAssetInput p -> Command.FileSource (p ^. unOriginalPath)
        SilenceInput duration' -> Command.AudioNullSource duration'
    toStreamNameOnlySelector streamName =
      Command.StreamSelector (Command.StreamName streamName) Nothing Nothing
    toVideoInputChain ::
         (PartStreamName, PartStream Video) -> Command.FilterChain
    toVideoInputChain (streamName, partStream) =
      case partStream of
        VideoClipStream ii ts ->
          trimmedIndexedInput Video streamName ii ts
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
          trimmedIndexedInput Audio streamName ii ts
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
    trimmedIndexedInput mediaType streamName (InputIndex i) ts =
      case mediaType of
        Video -> Command.FilterChain (trimFilter Command.Video Command.Trim :| [scaleFilter, setPTSFilter Command.SetPTSStart])
        Audio -> Command.FilterChain (trimFilter Command.Audio Command.AudioTrim :| [setPTSFilter Command.AudioSetPTSStart])
      where
        trimFilter track trim =
          Command.RoutedFilter
           [ Command.StreamSelector
               (Command.StreamIndex i)
               (Just track)
               (Just 0)
           ]
           (trim (spanStart ts) (durationOf ts))
           []
        scaleFilter =
          Command.RoutedFilter
             []
             Command.Scale
             { scaleWidth = videoSettings ^. resolution . width
             , scaleHeight = videoSettings ^. resolution . height
             , scaleForceOriginalAspectRatio =
                 Command.ForceOriginalAspectRatioDisable
             }
             []
        setPTSFilter setPTS =
          Command.RoutedFilter
             []
             setPTS
             [ Command.StreamSelector
                 (Command.StreamName streamName)
                 Nothing
                 Nothing
             ]

renderComposition
  :: (MonadIO m, MonadSafe m)
  => VideoSettings
  -> Source Video
  -> Command.Output
  -> Composition
  -> Producer ProgressUpdate m ()
renderComposition videoSettings videoSource target c@(Composition video audio) = do
  -- TODO: bracket to make sure temp directory is deleted
  canonical <- liftIO getCanonicalTemporaryDirectory
  tmpDir <- liftIO (createTempDirectory canonical "fastcut.render")
  videoInput <-
    liftIO (toCommandInput SVideo tmpDir videoSettings videoSource 0 video)
  audioInput <-
    liftIO
      (toCommandInput
         SAudio
         tmpDir
         videoSettings
         AudioOriginal
         (length (inputs videoInput))
         audio)
  let renderCmd = toRenderCommand videoSettings target videoInput audioInput
  runFFmpegCommand (ProgressUpdate "Rendering") (durationOf c) renderCmd
