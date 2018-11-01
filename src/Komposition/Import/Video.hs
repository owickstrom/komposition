{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
module Komposition.Import.Video where

import           Komposition.Prelude        hiding (catch)

import           Codec.FFmpeg               hiding (resolution)
import           Codec.FFmpeg.Encode
import qualified Codec.FFmpeg.Probe         as Probe
import           Codec.Picture              as CP
import           Control.Lens
import           Control.Monad.Catch
import qualified Data.Massiv.Array          as A
import           Data.Massiv.Array.IO       as A hiding (Image)
import           Data.Maybe                 (fromMaybe)
import           Data.Time.Clock
import qualified Data.Vector                as V
import qualified Data.Vector.Generic        as VG
import           Graphics.ColorSpace        as A
import           Pipes                      (Consumer', Pipe, Producer, (>->))
import qualified Pipes
import qualified Pipes.Parse                as Pipes
import qualified Pipes.Prelude              as Pipes hiding (show)
import           Pipes.Safe
import           System.Directory
import           System.FilePath
import           System.IO                  hiding (putStrLn)
import           Text.Printf

import           Komposition.Duration
import           Komposition.FFmpeg.Command (Command (Command))
import qualified Komposition.FFmpeg.Command as Command
import           Komposition.FFmpeg.Process
import           Komposition.Library
import           Komposition.Progress
import           Komposition.VideoSettings

initialize :: IO ()
initialize = initFFmpeg

data VideoImportError
  = UnexpectedError FilePath Text
  deriving (Show, Eq)

instance Exception VideoImportError

-- | The type of frames returned by "Codec.FFmpeg", i.e. JuicyPixel
-- images.
type Frame = Image PixelRGB8

-- | Video time is returned as a 'Double' by "Codec.FFmpeg".
type Time = Double

-- | We convert JuicyPixel images to Massiv arrays, as defined in the
-- @massiv-io@ package, to do parallel comparison over arrays.
type RGB8Frame = A.Array A.S A.Ix2 (A.Pixel RGB Word8)

data Timed a = Timed
  { untimed :: a
  , time    :: Time
  } deriving (Eq, Show, Functor)

readVideoFile :: MonadIO m => FilePath -> Producer (Timed Frame) m ()
readVideoFile filePath = do
  (getFrame, cleanup) <- liftIO (imageReaderTime (File filePath))
  yieldNext getFrame cleanup
  where
    yieldNext ::
         MonadIO m
      => IO (Maybe (Frame, Time))
      -> IO end
      -> Producer (Timed Frame) m end
    yieldNext getFrame cleanup = do
      m <- liftIO getFrame
      case m of
        Just (f, t) -> do
          Pipes.yield (Timed f t)
          yieldNext getFrame cleanup
        Nothing -> liftIO cleanup

toMassiv :: MonadIO m => Pipe (Timed Frame) (Timed RGB8Frame) m ()
toMassiv =
  Pipes.map $ \(Timed f t) ->
          f
          & A.fromDynamicImage . CP.ImageRGB8
          & fromMaybe (panic "Could not convert image")
          & A.setComp A.Par
          & flip Timed t

fromMassiv :: (MonadIO m) => Pipe (Timed RGB8Frame) (Timed Frame) m ()
fromMassiv = Pipes.map (fmap A.toJPImageRGB8)

dropTime :: Monad m => Pipe (Timed Frame) Frame m ()
dropTime = Pipes.map untimed

-- Compares all pixels using 'eps', counts the percent of equal ones,
-- and checks if the count exceeds the 'minEqPct'.
equalFrame :: Word8 -> Double -> RGB8Frame -> RGB8Frame ->  Bool
equalFrame eps minEqPct f1 f2 =
  pct > minEqPct
  where
    cmp :: A.Pixel RGB Word8 -> A.Pixel RGB Word8 -> Int
    cmp px1 px2 = if A.eqTolPx eps px1 px2 then 1 else 0
    {-# INLINE cmp #-}
    sumEq = A.sum (A.zipWith cmp f1 f2)
    total = A.totalElem (A.size f1)
    pct = fromIntegral sumEq / fromIntegral total

-- Compares every 8th row of pixels using 'eps', and checks if the
-- equal pixels in each row exceeds 'minEqPct'. Slightly faster than
-- 'equalFrame', especially since it can do early return, but also
-- less accurate (false positives).
equalFrame' :: HasCallStack => Word8 -> Double -> RGB8Frame -> RGB8Frame ->  Bool
equalFrame' eps minEqPct f1 f2 =
  A.size f1 == A.size f2 && cmpRow 0
  where
    (A.Ix2 height' width') = min (A.size f1) (A.size f2)
    step = 8
    minEqPxs = floor (fromIntegral width' * minEqPct)
    cmpRow i
      | i < height' =
        let sumEq :: Int32
            sumEq = A.sum (A.zipWith cmp (f1 A.!> i) (f2 A.!> i))
        in sumEq > minEqPxs && cmpRow (i + step)
      | otherwise = True
    cmp px1 px2 = if A.eqTolPx eps px1 px2 then 1 else 0
    {-# INLINE cmp #-}

data Classified f
  = Moving f
  | Still f
  deriving (Eq, Functor, Show)

unClassified :: Classified f -> f
unClassified = \case
  Moving f -> f
  Still f -> f

data ClassifierState
  = InMoving { equalFrames :: !(V.Vector (Timed RGB8Frame)) }
  | InStill { stillFrames     :: !(V.Vector (Timed RGB8Frame)) }

yield' :: Monad m => b -> Pipes.StateT (Producer a m x) (Producer b m) ()
yield' = lift . Pipes.yield

draw' :: Monad m => Pipes.StateT (Producer a m x) (Producer b m) (Maybe a)
draw' = Pipes.hoist lift Pipes.draw

classifyMovement :: Monad m => Time -> Producer (Timed RGB8Frame) m () -> Producer (Classified (Timed RGB8Frame)) m ()
classifyMovement minStillSegmentTime =
  Pipes.evalStateT $
  draw' >>= \case
    Just frame -> go (InMoving (VG.singleton frame))
    Nothing -> pure ()
  where
    minEqualTimeForStill = 0.5
    go ::
         Monad m
      => ClassifierState
      -> Pipes.StateT (Producer (Timed RGB8Frame) m ()) (Producer (Classified (Timed RGB8Frame)) m) ()
    go state' =
       (state',) <$> draw' >>= \case
        (InMoving {..}, Just frame)
          | equalFrame 1 0.999 (untimed frame) (untimed (VG.head equalFrames)) ->
            if time frame - time (VG.head equalFrames) > minEqualTimeForStill
              then do
                VG.mapM_ (yield' . Moving) equalFrames
                go (InStill (VG.singleton frame))
              else go (InMoving (VG.snoc equalFrames frame))
          | otherwise -> do
            VG.mapM_ (yield' . Moving) equalFrames
            go (InMoving (VG.singleton frame))
        (InMoving {..}, Nothing) -> VG.mapM_ (yield' . Moving) equalFrames
        (InStill {..}, Just frame)
          | equalFrame 1 0.999 (untimed (VG.head stillFrames)) (untimed frame) -> do
            go (InStill (VG.snoc stillFrames frame))
          | otherwise -> do
            let yieldFrame =
                  if time (VG.last stillFrames) - time (VG.head stillFrames) >= minStillSegmentTime
                  then yield' . Still
                  else yield' . Moving
            VG.mapM_ yieldFrame stillFrames
            go (InMoving (VG.singleton frame))
        (InStill {..}, Nothing) ->
            VG.mapM_ (yield' . Still) stillFrames

data SplitSegment a = SplitSegment
  { segmentNumber :: Int
  , segmentFrame  :: a
  } deriving (Eq, Show, Functor)

data PaddedSplitterState a
  = PaddedSplitterInStill Int !a
  | PaddedSplitterInMoving Int

printProcessingInfo :: MonadIO m => Consumer' (Timed f) m ()
printProcessingInfo =
  Pipes.mapM_ $ \(Timed _ n) ->
    liftIO $ do
      let s = floor n :: Int
      printf
        "\rProcessing at %02d:%02d:%02d"
        (s `div` 3600)
        (s `div` 60)
        (s `mod` 60)
      hFlush stdout

writeSplitVideoFiles ::
     MonadIO m
  => VideoSettings
  -> FilePath
  -> Producer (Classified (Timed Frame)) m ()
  -> Producer (Classified (Timed Frame)) m [FilePath]
writeSplitVideoFiles settings outDir = Pipes.evalStateT (go (Left (), 1 :: Int, []))
  where
    go state' =
      draw' >>= \case
        Just frame -> do
          yield' frame
          case (state', frame) of
            ((Left (), n, files), Still _) -> go (Left (), n, files)
            ((Left (), n, files), Moving f) -> do
              let ep = (defaultH264
                          (fromIntegral (settings ^. resolution . width))
                          (fromIntegral (settings ^. resolution . height)))
                       {epFps = fromIntegral (settings ^. frameRate)}
                  filePath = outDir </> show n ++ ".mp4"
              writeFrame <- liftIO (imageWriter ep filePath)
              liftIO (writeFrame (Just (untimed f)))
              go (Right writeFrame, n, filePath : files)
            ((Right writeFrame, n, files), Still _) -> do
              liftIO (writeFrame Nothing)
              go (Left (), succ n, files)
            ((Right writeFrame, n, files), Moving (untimed -> f)) -> do
              liftIO (writeFrame (Just f))
              go (Right writeFrame, n, files)
        Nothing ->
          case state' of
            (Left (), _, files) ->
              return (reverse files)
            (Right writeFrame, _, files) -> do
              liftIO (writeFrame Nothing)
              return (reverse files)

classifyMovingScenes ::
     Monad m
  => Duration
  -> Producer (Classified (Timed RGB8Frame)) m ()
  -> Producer ProgressUpdate m [TimeSpan]
classifyMovingScenes fullLength =
  Pipes.evalStateT $
  draw' >>= \case
    Just (Still _) -> go (Left (), [])
    Just (Moving _) -> go (Right (0, 0), [])
    Nothing -> return []
  where
    go state' =
      draw' >>= \case
        Just frame -> do
          yield' (toProgress (time (unClassified frame)))
          case (state', frame) of
            ((Left (), spans), Still _) -> go (Left (), spans)
            ((Left (), spans), Moving f) -> go (Right (time f, time f), spans)
            ((Right (firstTime, _), spans), Still f) ->
              go
                ( Left ()
                , spans <>
                  [ TimeSpan
                      (durationFromSeconds firstTime)
                      (durationFromSeconds (time f))
                  ])
            ((Right (firstTime, _), spans), Moving f) ->
              go (Right (firstTime, time f), spans)
        Nothing ->
          case state' of
            (Left (), spans) -> return spans
            (Right (startTime, _), spans) ->
              return $
              spans <>
              [ TimeSpan
                  (durationFromSeconds startTime)
                  fullLength
              ]
    toProgress time =
      ProgressUpdate "Classifying scenes" (time / durationToSeconds fullLength)

getVideoFileDuration :: (MonadMask m, MonadIO m) => FilePath -> m Duration
getVideoFileDuration f =
  Duration . picosecondsToDiffTime . (* 1000000) . fromIntegral <$>
  Probe.withAvFile f Probe.duration

filePathToVideoAsset ::
     (MonadMask m, MonadSafe m, MonadIO m)
  => VideoSettings
  -> FilePath
  -> OriginalPath
  -> Producer ProgressUpdate m VideoAsset
filePathToVideoAsset videoSettings outDir p = do
  d <- liftIO (getVideoFileDuration (p ^. unOriginalPath))
  proxyPath <- generateProxy videoSettings p d (outDir </> "proxies")
  pure (VideoAsset (AssetMetadata p d) proxyPath Nothing)

generateVideoThumbnail ::
     (MonadError VideoImportError m, MonadIO m)
  => FilePath
  -> FilePath
  -> m (Maybe FilePath)
generateVideoThumbnail sourceFile outDir = do
  (readImage', cleanup) <- liftIO (imageReader (File sourceFile))
  liftIO readImage' >>= \case
    Just (img :: Image PixelRGB8) -> do
      let fileName = outDir </> replaceExtension (snd (splitFileName sourceFile)) "png" <> ""
      liftIO (writePng fileName img)
      liftIO cleanup
      pure (Just fileName)
    Nothing -> do
      liftIO cleanup
      pure Nothing

newtype AutoSplit = AutoSplit Bool deriving (Show, Eq)

importVideoFile ::
     (MonadIO m, MonadSafe m)
  => VideoSettings
  -> FilePath
  -> FilePath
  -> Producer ProgressUpdate m VideoAsset
importVideoFile settings sourceFile outDir = do
  -- Copy asset to working directory
  assetPath <- liftIO $ do
    createDirectoryIfMissing True outDir
    let assetPath = outDir </> takeFileName sourceFile
    copyFile sourceFile assetPath
    return (OriginalPath assetPath)
  -- Generate thumbnail and return asset
  filePathToVideoAsset settings outDir assetPath

generateProxy ::
     (MonadIO m, MonadSafe m)
  => VideoSettings
  -> OriginalPath
  -> Duration
  -> FilePath
  -> Producer ProgressUpdate m ProxyPath
generateProxy videoSettings (view unOriginalPath -> sourceFile) fullLength outDir = do
  liftIO (createDirectoryIfMissing True outDir)
  let proxyPath = outDir </> takeBaseName sourceFile <> ".proxy.mp4"
      cmd =
        Command
        { output = Command.FileOutput proxyPath
        , inputs = pure (Command.FileSource sourceFile)
        , filterGraph =
            Just . Command.FilterGraph $
              pure
                (Command.FilterChain
                   (pure
                      (Command.RoutedFilter
                         []
                         (Command.Scale
                            640
                            360
                            Command.ForceOriginalAspectRatioDisable)
                         [])))
        , frameRate = Just (videoSettings ^. frameRate)
        , mappings = []
        , vcodec = Just "h264"
        , acodec = Nothing
        , format = Just "mp4"
        }
  runFFmpegCommand (ProgressUpdate "Generating proxy") fullLength cmd
  return (ProxyPath proxyPath)

importVideoFileAutoSplit ::
     (MonadIO m, MonadSafe m)
  => VideoSettings
  -> FilePath
  -> FilePath
  -> Producer ProgressUpdate m [VideoAsset]
importVideoFileAutoSplit settings sourceFile outDir = do
  fullLength <- liftIO (getVideoFileDuration sourceFile)
  generateProxy settings original fullLength (outDir </> "proxies")
    & flip divideProgress2 (classifyScenes fullLength)
  where
    original = OriginalPath sourceFile
    toSceneAsset proxyPath fullLength n timeSpan = do
      let meta = AssetMetadata original fullLength
      return (VideoAsset meta proxyPath (Just (n, timeSpan)))
    classifyScenes fullLength proxyPath =
      classifyMovement 1.0 (readVideoFile (proxyPath ^. unProxyPath) >-> toMassiv)
        & classifyMovingScenes fullLength
        & (>>= zipWithM (toSceneAsset proxyPath fullLength) [1 ..])

isSupportedVideoFile :: FilePath -> Bool
isSupportedVideoFile p = takeExtension p `elem` [".mp4", ".m4v", ".webm", ".avi", ".mkv", ".mov", ".flv"]

split :: VideoSettings -> Time -> FilePath -> FilePath -> IO ()
split settings equalFramesTimeThreshold src outDir = do
  createDirectoryIfMissing True outDir
  let classified =
        classifyMovement
          equalFramesTimeThreshold
          (readVideoFile src >-> toMassiv) >->
        Pipes.tee (Pipes.map unClassified >-> printProcessingInfo)
      writeSplitSegments =
        classified >-> Pipes.map (fmap (fmap A.toJPImageRGB8)) &
        writeSplitVideoFiles settings outDir
      discardUpdates = forever Pipes.await
      printResult res = do
        putStrLn ("" :: Text)
        case res of
          Left (err :: VideoImportError) -> putStrLn ("Video split failed: " <> show err :: Text)
          Right files ->
            putStrLn ("Wrote " <> show (length files) <> " files." :: Text)
  runSafeT (Pipes.runEffect (tryP (writeSplitSegments >-> discardUpdates) >>= printResult))
