{-# LANGUAGE RankNTypes            #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
module FastCut.Import.Video where

import           FastCut.Prelude         hiding (catch)

import           Codec.FFmpeg            hiding (resolution)
import           Codec.FFmpeg.Encode
import qualified Codec.FFmpeg.Probe      as Probe
import           Codec.Picture           as CP
import           Codec.Picture.Types     as CP
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Primitive
import qualified Data.Massiv.Array       as A
import           Data.Massiv.Array.IO    as A hiding (Image)
import           Data.Maybe              (fromMaybe)
import qualified Data.Text               as Text
import           Data.Time.Clock
import qualified Data.Vector             as V
import qualified Data.Vector.Generic     as VG
import           Graphics.ColorSpace     as A
import           Pipes                   (Consumer', Pipe, Producer, (>->))
import qualified Pipes
import qualified Pipes.Lift              as Pipes
import qualified Pipes.Parse             as Pipes
import qualified Pipes.Prelude           as Pipes hiding (show)
import           System.Directory
import           System.FilePath
import           System.IO               hiding (putStrLn)
import           System.Process
import           Text.Printf

import           FastCut.Duration
import           FastCut.Library
import           FastCut.Progress
import           FastCut.VideoSettings

initialize :: IO ()
initialize = initFFmpeg

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

readVideoFile :: MonadIO m => FilePath -> Producer (Timed Frame) (ExceptT VideoImportError m) ()
readVideoFile filePath = do
  (getFrame, cleanup) <-
    lift ((UnexpectedError filePath . toS) `withExceptT` imageReaderTimeT (File filePath))
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


colorClassifiedMovement :: (PrimMonad m, MonadIO m) => Pipe (Classified (Timed RGB8Frame)) (Timed RGB8Frame) m ()
colorClassifiedMovement =
  Pipes.mapM $ \case
    Moving frame -> tint' green frame
    Still frame -> tint' red frame
  where
    tint' color frame =
      pure $ A.compute . A.map (\px -> meanWord8 <$> color <*> px) <$> frame
    red = PixelRGB 255 0 0
    green = PixelRGB 0 255 0

blend :: PixelRGB8 -> PixelRGB8 -> PixelRGB8
blend = mixWith (const meanWord8)

meanWord8 :: Word8 -> Word8 -> Word8
meanWord8 a b =
  let n = fromIntegral a + fromIntegral b :: Word16
  in fromIntegral (n `div` 2)

tint :: (PrimMonad m) => PixelRGB8 -> MutableImage (PrimState m) PixelRGB8 -> m ()
tint color frame =
  sequence_
  [ do p <- readPixel frame x y
       writePixel frame x y (blend color p)
  | x <- [0 .. pred (mutableImageWidth frame)]
  , y <- [0 .. pred (mutableImageHeight frame)]
  ]

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
  -> Producer (Classified (Timed Frame)) m ()
  -> Producer Time m [TimeSpan]
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
          yield' (time (unClassified frame))
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

getVideoFileDuration :: (MonadMask m, MonadIO m) => FilePath -> m Duration
getVideoFileDuration f =
  Duration . picosecondsToDiffTime . (* 1000000) . fromIntegral <$>
  Probe.withAvFile f Probe.duration

filePathToVideoAsset ::
     (MonadError VideoImportError m, MonadIO m)
  => FilePath
  -> FilePath
  -> m VideoAsset
filePathToVideoAsset outDir p = do
  d <- liftIO (getVideoFileDuration p)
  proxyPath <- liftIO (generateProxy p (outDir </> "proxies"))
  let meta = AssetMetadata p d
  pure (VideoAsset meta proxyPath Nothing)

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

data VideoImportError
  = UnexpectedError FilePath Text
  deriving (Show, Eq)

importVideoFile ::
     MonadIO m
  => FilePath
  -> FilePath
  -> Producer ProgressUpdate m (Either VideoImportError VideoAsset)
importVideoFile sourceFile outDir = do
  Pipes.yield (ProgressUpdate 0)
  -- Copy asset to working directory
  assetPath <- liftIO $ do
    createDirectoryIfMissing True outDir
    let assetPath = outDir </> takeFileName sourceFile
    copyFile sourceFile assetPath
    return assetPath
  -- Generate thumbnail and return asset
  Pipes.yield (ProgressUpdate 0.5) *>
    (filePathToVideoAsset outDir assetPath & runExceptT)
    <* Pipes.yield (ProgressUpdate 1)

generateProxy :: FilePath -> FilePath -> IO ProxyPath
generateProxy sourceFile outDir = do
  createDirectoryIfMissing True outDir
  let proxyPath = outDir </> takeBaseName sourceFile <> ".proxy.mp4"
      allArgs =
        [ "-nostdin"
        , "-i"
        , sourceFile
        , "-vf"
        , "scale=640:-1"
        , "-vcodec"
        , "h264"
        , "-crf"
        , "18"
        , proxyPath
        ]
  putStrLn $ Text.unwords ("ffmpeg" : map toS allArgs)
  (exit, _, err) <- readCreateProcessWithExitCode (proc "ffmpeg" allArgs) ""
  when
    (exit /= ExitSuccess)
    (panic ("Couldn't generate proxy video from file: " <> toS err))
  return (ProxyPath proxyPath)

importVideoFileAutoSplit ::
     MonadIO m
  => VideoSettings
  -> FilePath
  -> FilePath
  -> Producer ProgressUpdate m (Either VideoImportError [VideoAsset])
importVideoFileAutoSplit _settings sourceFile outDir = do
  fullLength <- liftIO (getVideoFileDuration sourceFile)
  proxyPath <- liftIO (generateProxy sourceFile (outDir </> "proxies"))
  let classifiedFrames =
        classifyMovement
          1.0
          (readVideoFile (proxyPath ^. unProxyPath) >-> toMassiv) >->
        Pipes.map (fmap (fmap A.toJPImageRGB8))
  classifyMovingScenes fullLength classifiedFrames >->
    Pipes.map (toProgress fullLength) &
    (>>= zipWithM (toSceneAsset proxyPath fullLength) [1 ..]) &
    Pipes.runExceptP
  where
    toProgress (durationToSeconds -> fullLength) time =
      ProgressUpdate (time / fullLength)
    toSceneAsset proxyPath fullLength n timeSpan = do
      let meta = AssetMetadata sourceFile fullLength
      return (VideoAsset meta proxyPath (Just (n, timeSpan)))

isSupportedVideoFile :: FilePath -> Bool
isSupportedVideoFile p = takeExtension p `elem` [".mp4", ".m4v", ".webm", ".avi", ".mkv", ".mov"]

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
          Left _err -> putStrLn ("Failed." :: Text)
          Right files ->
            putStrLn ("Wrote " <> show (length files) <> " files." :: Text)
  Pipes.runEffect
    (Pipes.runExceptP writeSplitSegments >-> discardUpdates >>= printResult)
