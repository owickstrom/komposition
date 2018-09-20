{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module FastCut.FFmpeg.Command
  ( Command(..)
  , Source(..)
  , Name
  , FrameRate
  , StreamType(..)
  , StreamIdentifier(..)
  , StreamSelector(..)
  , ForceOriginalAspectRatio(..)
  , Filter(..)
  , RoutedFilter(..)
  , FilterChain(..)
  , FilterGraph(..)
  , printCommandLineArgs
  )
where

import           FastCut.Prelude
import qualified Prelude

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text          as Text
import           Text.Printf

import           FastCut.Duration
import           FastCut.Timestamp

data Command = Command
  { inputs      :: NonEmpty Source
  , filterGraph :: FilterGraph
  , frameRate   :: Maybe FrameRate
  , mappings    :: [StreamSelector]
  , format      :: Text
  , vcodec      :: Maybe Text
  , acodec      :: Maybe Text
  , output      :: FilePath
  }

data Source
  = FileSource FilePath
  | StillFrameSource FilePath FrameRate Duration
  | AudioNullSource Duration

type Name = Text

type FrameRate = Word

type Index = Integer

data StreamType = Video | Audio | Subtitle

data StreamIdentifier
  = StreamIndex Index
  | StreamName Name

data StreamSelector = StreamSelector
  { streamIdentifier :: StreamIdentifier
  , streamType       :: Maybe StreamType
  , streamIndex      :: Maybe Index
  }

data ForceOriginalAspectRatio
  = ForceOriginalAspectRatioDisable
  | ForceOriginalAspectRatioDecrease
  | ForceOriginalAspectRatioIncrease

data Filter
  = Concat { concatSegments     :: Integer
           , concatVideoStreams :: Integer
           , concatAudioStreams :: Integer }
  | Scale { scaleWidth                    :: Word
          , scaleHeight                   :: Word
          , scaleForceOriginalAspectRatio :: ForceOriginalAspectRatio }
  | Trim { trimStart    :: Duration
         , trimDuration :: Duration }
  | SetPTSStart
  | AudioSetPTSStart

data RoutedFilter = RoutedFilter
  { filterInputs  :: [StreamSelector]
  , routedFilter  :: Filter
  , filterOutputs :: [StreamSelector]
  }

data FilterChain = FilterChain (NonEmpty RoutedFilter)

data FilterGraph = FilterGraph (NonEmpty FilterChain)

printCommandLineArgs :: Command -> [Text]
printCommandLineArgs Command {..} =
  concatMap printSourceArgs inputs
    <> ["-filter_complex", printFilterGraph filterGraph]
    <> foldMap printMapping mappings
    <> ["-f", format]
    <> printOptionalPair "-framerate" show frameRate
    <> printOptionalPair "-vcodec" identity vcodec
    <> printOptionalPair "-acodec" identity acodec
    <> [toS output]

printOptionalPair :: Text -> (a -> Text) -> Maybe a -> [Text]
printOptionalPair flag printValue = \case
  Just value -> [flag, printValue value]
  Nothing -> []

printSourceArgs :: Source -> [Text]
printSourceArgs =
  \case
    FileSource path -> ["-i", toS path]
    StillFrameSource path frameRate duration ->
      [ "-loop"
      , "1"
      , "-framerate"
      , show frameRate
      , "-t"
      , printTimestamp duration
      , "-i"
      , toS path
      ]
    AudioNullSource d ->
      [ "-f"
      , "lavfi"
      , "-i"
      , "aevalsrc=0:duration=" <> show (durationToSeconds d)
      ]

printMapping :: StreamSelector -> [Text]
printMapping sel = ["-map", encloseInBrackets (printStreamSelector sel)]

printFilterGraph :: FilterGraph -> Text
printFilterGraph (FilterGraph chains) = Text.intercalate
  ";"
  (NonEmpty.toList (map printFilterChain chains))
  where
    printFilterChain (FilterChain calls) =
      Text.intercalate "," (NonEmpty.toList (map printRoutedFilter calls))

    printRoutedFilter RoutedFilter {..} =
      foldMap (encloseInBrackets . printStreamSelector) filterInputs
        <> printFilter routedFilter
        <> foldMap (encloseInBrackets . printStreamSelector) filterOutputs

printForceOriginalAspectRatio :: ForceOriginalAspectRatio -> Text
printForceOriginalAspectRatio = \case
  ForceOriginalAspectRatioDisable -> "disable"
  ForceOriginalAspectRatioDecrease -> "decrease"
  ForceOriginalAspectRatioIncrease -> "increase"

printFilter :: Filter -> Text
printFilter =
  \case
    Concat {..} ->
      toS
        (printf
           "concat=n=%d:v=%d:a=%d"
           concatSegments
           concatVideoStreams
           concatAudioStreams :: Prelude.String)
    Scale {..} ->
      toS
        (printf
           "scale=width=%d:height=%d:force_original_aspect_ratio=%s"
           scaleWidth
           scaleHeight
           (printForceOriginalAspectRatio scaleForceOriginalAspectRatio) :: Prelude.String)
    Trim {..} ->
      toS
        (printf
           "trim=start=%f:duration=%f"
           -- NOTE: the trim filter appearantly doesn't handle
           -- timestamp format well, so we format as seconds with
           -- decimal numbers
           (durationToSeconds trimStart)
           (durationToSeconds trimDuration) :: Prelude.String)
    SetPTSStart -> "setpts=PTS-STARTPTS"
    AudioSetPTSStart -> "asetpts=PTS-STARTPTS"

printStreamSelector :: StreamSelector -> Text
printStreamSelector StreamSelector {..} =
  printStreamIdentifier streamIdentifier
    <> maybe "" ((":" <>) . printStreamType) streamType
    <> maybe "" ((":" <>) . show)            streamIndex

printStreamType :: StreamType -> Text
printStreamType = \case
  Video    -> "v"
  Audio    -> "a"
  Subtitle -> "s"
printStreamIdentifier :: StreamIdentifier -> Text
printStreamIdentifier = \case
  StreamIndex i -> show i
  StreamName  n -> n

encloseInBrackets :: Text -> Text
encloseInBrackets t = "[" <> t <> "]"
