{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module FastCut.Render.FFmpeg where

import           FastCut.Prelude
import qualified Prelude

import           Control.Lens
import qualified Data.List.NonEmpty         as NonEmpty
import qualified Data.Text                  as Text
import qualified Data.Text.Read             as Text
import           System.FilePath
import           System.IO.Temp
import           System.Process
import           Text.Printf

import           FastCut.Duration
import           FastCut.Library
import           FastCut.MediaType
import           FastCut.Render.Composition (Composition (..))
import qualified FastCut.Render.Composition as Composition

type FrameRate = Word

data Part mt where
  Clip :: Asset mt -> Part mt
  StillFrame :: FilePath -> FrameRate -> Duration -> Part Video
  Silence :: Duration -> Part Audio

type IndexedPart mt = (Int, Part mt)

extractNumberOfFrames ::  FilePath -> IO Integer
extractNumberOfFrames videoFile  = do
  let probeCommand = proc "ffprobe" ["-show_streams", videoFile]
  (exit, sout, _) <- readCreateProcessWithExitCode probeCommand ""
  when (exit /= ExitSuccess) (Prelude.fail ("Couldn't read number of frames in video file: " <> videoFile))
  Text.pack sout
    & Text.lines
    & mapMaybe parseKeyValue
    & find ((== "nb_frames") . fst)
    & map snd
    & (>>= readInt)
    & maybe (Prelude.fail "Couldn't parse number of frames from ffprobe output") return
  where
    parseKeyValue t =
      case Text.splitOn "=" t of
        [key, value] -> Just (key, value)
        _            -> Nothing
    readInt t =
      case Text.decimal t of
        Left _err    -> Nothing
        Right (n, _) -> Just n


extractFrameToFile :: Composition.StillFrameMode -> Asset Video -> FilePath -> IO ()
extractFrameToFile mode (VideoAsset meta) frameFile = do
  let sourcePath = meta ^. path
  frameIndex <-
    case mode of
      Composition.FirstFrame -> return 0
      Composition.LastFrame  -> pred <$> extractNumberOfFrames sourcePath
  putStrLn
    ("Extracting frame " <> show frameIndex <> " of file " <> sourcePath <> " to " <> frameFile)
  let ffmpegCommand =
        proc
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
       ("Couldn't extract frame from video file (" <> sourcePath <> "): " <>
        err))

toIndexedParts ::
     FilePath
  -> FrameRate
  -> NonEmpty (Composition.CompositionPart mt)
  -> IO (NonEmpty (IndexedPart mt))
toIndexedParts tmpDir frameRate =
  traverse toPart . NonEmpty.zip (0 :| [1 ..])
  where
    toPart ::
      (Int, Composition.CompositionPart mt)
      -> IO (IndexedPart mt)
    toPart =
      \case
        (i, Composition.Clip asset) -> return (i, Clip asset)
        (i, Composition.StillFrame mode asset duration') -> do
          let frameFile = tmpDir </> show i <> ".png"
          extractFrameToFile mode asset frameFile
          return (i, StillFrame frameFile frameRate duration')
        (i, Composition.Silence duration') -> return (i, Silence duration')

formatTimestamp :: Duration -> Prelude.String
formatTimestamp d =
  let sec = durationToSeconds d
      hours, minutes, seconds :: Int
      hours = floor (sec / 3600)
      minutes = floor (sec / 3600)
      seconds = floor sec
      milliseconds = sec - fromIntegral seconds
      millisecondsFractionPart =
        drop 1 (printf "%f" milliseconds) :: Prelude.String
  in printf "%02d:%02d:%02d%s" hours minutes seconds millisecondsFractionPart

renderVideoCommand :: FilePath -> NonEmpty (IndexedPart Video) -> CreateProcess
renderVideoCommand outFile parts
  -- https://stackoverflow.com/questions/43958438/merge-videos-and-images-using-ffmpeg#
  --
  -- ffmpeg \
  --   -loop 1 -framerate 24 -t 10 -i image1.jpg \
  --   -i video.mp4 \
  --   -loop 1 -framerate 24 -t 10 -i image2.jpg \
  --   -loop 1 -framerate 24 -t 10 -i image3.jpg \
  --   -filter_complex "[0][1][2][3]concat=n=4:v=1:a=0" out.mp4
 = proc "ffmpeg" ("-nostdin" : partParams <> filterComplex <> outputParams)
  where
    partParams, filterComplex, outputParams :: [Prelude.String]
    partParams =
      flip concatMap (NonEmpty.toList parts) $ \case
        (_, Clip (VideoAsset asset)) -> ["-i", asset ^. path]
        (_, StillFrame imagePath frameRate duration') ->
          [ "-loop"
          , "1"
          , "-framerate"
          , show frameRate
          , "-t"
          , formatTimestamp duration'
          , "-i"
          , imagePath
          ]
    filterComplex =
      [ "-filter_complex"
      , concatMap (inBrackets . fst) parts <> "concat=n=" <> show (length parts) <>
        ":v=1:a=0"
      ]
    outputParams = ["-f", "mp4", outFile]
    inBrackets :: Show a => a -> Prelude.String
    inBrackets x = "[" <> show x <> "]"


renderComposition :: FrameRate -> FilePath -> Composition -> IO ()
renderComposition frameRate outFile (Composition video _audio) = do
  canonical <- getCanonicalTemporaryDirectory
  tmpDir <- createTempDirectory canonical "fastcut.render"
  indexedVideo <- toIndexedParts tmpDir frameRate video
  _ <- readCreateProcess (renderVideoCommand outFile indexedVideo) ""
  return ()
