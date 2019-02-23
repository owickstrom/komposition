{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
module Komposition.Import.Video.FFmpeg
  ( initialize
  , readVideoFile
  , toMassiv
  , classifyMovement
  , classifyMovingScenes
  , getVideoFileDuration
  , Classified(..)
  , unClassified
  , Time
  , Timed(..)
  , JuicyFrame
  , MassivFrame
  , equalFrame
  , equalFrame'
  , runFFmpegVideoImport
  )
where

import           Komposition.Prelude        hiding (catch)

import           Codec.FFmpeg               hiding (resolution)
import qualified Codec.FFmpeg.Probe         as Probe
import           Codec.Picture              as CP
import           Control.Effect
import           Control.Effect.Carrier
import           Control.Effect.Sum
import           Control.Lens
import           Control.Monad.Catch
import qualified Data.Massiv.Array          as A
import           Data.Massiv.Array.IO       as A hiding (Image)
import           Data.Maybe                 (fromMaybe)
import           Data.Time.Clock
import qualified Data.Vector                as V
import qualified Data.Vector.Generic        as VG
import           Graphics.ColorSpace        as A
import           Pipes                      (Pipe, Producer, (>->))
import qualified Pipes
import qualified Pipes.Parse                as Pipes
import qualified Pipes.Prelude              as Pipes hiding (show)
import           Pipes.Safe
import           System.Directory
import           System.FilePath

import           Komposition.Classification
import           Komposition.Duration
import           Komposition.FFmpeg.Command (Command (Command))
import qualified Komposition.FFmpeg.Command as Command
import           Komposition.FFmpeg.Process
import           Komposition.Import.Video
import           Komposition.Library
import           Komposition.Progress
import           Komposition.VideoSettings
import           Komposition.VideoSpeed

initialize :: IO ()
initialize = initFFmpeg

-- | The type of frames returned by "Codec.FFmpeg", i.e. JuicyPixel
-- images.
type JuicyFrame = Image PixelRGB8

-- | Video time is returned as a 'Double' by "Codec.FFmpeg".
type Time = Double

-- | We convert JuicyPixel images to Massiv arrays, as defined in the
-- @massiv-io@ package, to do parallel comparison over arrays.
type MassivFrame = A.Array A.S A.Ix2 (A.Pixel RGB Word8)

data Timed a = Timed
  { untimed :: a
  , time    :: Time
  } deriving (Eq, Show, Functor)

readVideoFile :: MonadIO m => FilePath -> Producer (Timed JuicyFrame) m ()
readVideoFile filePath = do
  (getFrame, cleanup) <- liftIO (imageReaderTime (File filePath))
  yieldNext getFrame cleanup
  where
    yieldNext
      :: MonadIO m
      => IO (Maybe (JuicyFrame, Time))
      -> IO end
      -> Producer (Timed JuicyFrame) m end
    yieldNext getFrame cleanup = do
      m <- liftIO getFrame
      case m of
        Just (f, t) -> do
          Pipes.yield (Timed f t)
          yieldNext getFrame cleanup
        Nothing -> liftIO cleanup

toMassiv :: MonadIO m => Pipe (Timed JuicyFrame) (Timed MassivFrame) m ()
toMassiv = Pipes.map $ \(Timed f t) ->
  f
    & A.fromDynamicImage
    . CP.ImageRGB8
    & fromMaybe (panic "Could not convert image")
    & A.setComp A.Par
    & flip Timed t

-- Compares all pixels using 'eps', counts the percent of equal ones,
-- and checks if the count exceeds the 'minEqPct'.
equalFrame :: Word8 -> Double -> MassivFrame -> MassivFrame -> Bool
equalFrame eps minEqPct f1 f2 = pct > minEqPct
  where
    cmp :: A.Pixel RGB Word8 -> A.Pixel RGB Word8 -> Int
    cmp px1 px2 = if A.eqTolPx eps px1 px2 then 1 else 0
    {-# INLINE cmp #-}
    sumEq = A.sum (A.zipWith cmp f1 f2)
    total = A.totalElem (A.size f1)
    pct   = fromIntegral sumEq / fromIntegral total

-- Compares every 8th row of pixels using 'eps', and checks if the
-- equal pixels in each row exceeds 'minEqPct'. Slightly faster than
-- 'equalFrame', especially since it can do early return, but also
-- less accurate (false positives).
equalFrame'
  :: HasCallStack => Word8 -> Double -> MassivFrame -> MassivFrame -> Bool
equalFrame' eps minEqPct f1 f2 = A.size f1 == A.size f2 && cmpRow 0
  where
    (A.Ix2 height' width') = min (A.size f1) (A.size f2)
    step                   = 8
    minEqPxs               = floor (fromIntegral width' * minEqPct)
    cmpRow i
      | i < height'
      = let sumEq :: Int32
            sumEq = A.sum (A.zipWith cmp (f1 A.!> i) (f2 A.!> i))
        in  sumEq > minEqPxs && cmpRow (i + step)
      | otherwise
      = True
    cmp px1 px2 = if A.eqTolPx eps px1 px2 then 1 else 0
    {-# INLINE cmp #-}

data Classified f
  = Moving f
  | Still f
  deriving (Eq, Functor, Show)

unClassified :: Classified f -> f
unClassified = \case
  Moving f -> f
  Still  f -> f

data ClassifierState
  = InMoving { lastFrame :: !(Timed MassivFrame) }
  | InStill { stillFrames :: !(V.Vector (Timed MassivFrame)) }

yield' :: Monad m => b -> Pipes.StateT (Producer a m x) (Producer b m) ()
yield' = lift . Pipes.yield

draw' :: Monad m => Pipes.StateT (Producer a m x) (Producer b m) (Maybe a)
draw' = Pipes.hoist lift Pipes.draw

classifyMovement
  :: Monad m
  => Time
  -> Producer (Timed MassivFrame) m ()
  -> Producer (Classified (Timed MassivFrame)) m ()
classifyMovement minStillSegmentTime = Pipes.evalStateT $ draw' >>= \case
  Just frame -> go (InMoving frame)
  Nothing    -> pure ()
  where
    go
      :: Monad m
      => ClassifierState
      -> Pipes.StateT
           (Producer (Timed MassivFrame) m ())
           (Producer (Classified (Timed MassivFrame)) m)
           ()
    go state' = (state', ) <$> draw' >>= \case
      (InMoving {..}, Just frame)
        | equalFrame 1 0.99 (untimed frame) (untimed lastFrame) -> go
          (InStill (VG.fromList [lastFrame, frame]))
        | otherwise -> do
          yield' (Moving lastFrame)
          go (InMoving frame)
      (InMoving {..}, Nothing) -> yield' (Moving lastFrame)
      (InStill {..}, Just frame)
        | equalFrame 1 0.999 (untimed (VG.head stillFrames)) (untimed frame) -> go
          (InStill (VG.snoc stillFrames frame))
        | otherwise -> do
          let diff' = time (VG.last stillFrames) - time (VG.head stillFrames)
              yieldFrame = if diff' >= minStillSegmentTime
                then yield' . Still
                else yield' . Moving
          VG.mapM_ yieldFrame stillFrames
          go (InMoving frame)
      (InStill {..}, Nothing) -> VG.mapM_ (yield' . Still) stillFrames

data SplitSegment a = SplitSegment
  { segmentNumber :: Int
  , segmentFrame  :: a
  } deriving (Eq, Show, Functor)

classifyMovingScenes
  :: Monad m
  => Duration
  -> Producer (Classified (Timed MassivFrame)) m ()
  -> Producer ProgressUpdate m [TimeSpan]
classifyMovingScenes fullLength = Pipes.evalStateT $ draw' >>= \case
  Just (Still  _) -> go (Left (), [])
  Just (Moving _) -> go (Right (0, 0), [])
  Nothing         -> return []
  where
    go state' = draw' >>= \case
      Just frame -> do
        yield' (toProgress (time (unClassified frame)))
        case (state', frame) of
          ((Left (), spans), Still _) -> go (Left (), spans)
          ((Left (), spans), Moving f) -> go (Right (time f, time f), spans)
          ((Right (firstTime, _), spans), Still f) -> go
            ( Left ()
            , spans
              <> [ TimeSpan (durationFromSeconds firstTime)
                            (durationFromSeconds (time f))
                 ]
            )
          ((Right (firstTime, _), spans), Moving f) ->
            go (Right (firstTime, time f), spans)
      Nothing -> case state' of
        (Left (), spans) -> return spans
        (Right (startTime, _), spans) ->
          return
            $  spans
            <> [TimeSpan (durationFromSeconds startTime) fullLength]
    toProgress time =
      ProgressUpdate "Classifying scenes" (time / durationToSeconds fullLength)

getVideoFileDuration :: (MonadMask m, MonadIO m) => FilePath -> m Duration
getVideoFileDuration f =
  Duration
    .   picosecondsToDiffTime
    .   (* 1000000)
    .   fromIntegral
    <$> Probe.withAvFile f Probe.duration

filePathToVideoAsset
  :: (MonadMask m, MonadSafe m, MonadIO m)
  => AllVideoSettings
  -> VideoSpeed
  -> FilePath
  -> OriginalPath
  -> Producer ProgressUpdate m VideoAsset
filePathToVideoAsset vs defaultVideoSpeed outDir p = do
  d              <- liftIO (getVideoFileDuration (p ^. unOriginalPath))
  transcodedPath <- transcode' (vs ^. renderVideoSettings)
                               p
                               d
                               (outDir </> "transcoded")
                               "Transcoding video file"
  proxyPath <- transcode' (vs ^. proxyVideoSettings)
                          p
                          d
                          (outDir </> "proxies")
                          "Generating proxy video"
  pure
    (VideoAsset (AssetMetadata p d)
                transcodedPath
                proxyPath
                defaultVideoSpeed
                Nothing
    )

generateVideoThumbnail'
  :: (MonadIO m) => OriginalPath -> FilePath -> m (Maybe FilePath)
generateVideoThumbnail' (view unOriginalPath -> sourceFile) outDir = do
  (readImage', cleanup) <- liftIO (imageReader (File sourceFile))
  liftIO readImage' >>= \case
    Just (img :: Image PixelRGB8) -> do
      let fileName =
            outDir
              </> replaceExtension (snd (splitFileName sourceFile)) "png"
              <>  ""
      liftIO (writePng fileName img)
      liftIO cleanup
      pure (Just fileName)
    Nothing -> do
      liftIO cleanup
      pure Nothing

transcode'
  :: (MonadIO m, MonadSafe m)
  => VideoSettings
  -> OriginalPath
  -> Duration
  -> FilePath
  -> Text
  -> Producer ProgressUpdate m TranscodedPath
transcode' videoSettings (view unOriginalPath -> sourceFile) fullLength outDir progressMsg
  = do
    liftIO (createDirectoryIfMissing True outDir)
    let
      proxyPath = outDir </> takeBaseName sourceFile <> ".transcoded.mp4"
      cmd       = Command
        { output      = Command.FileOutput proxyPath
        , inputs      = pure (Command.FileSource sourceFile)
        , filterGraph =
          Just . Command.FilterGraph $ pure
            (Command.FilterChain
              (pure
                (Command.RoutedFilter
                  []
                  (Command.Scale (videoSettings ^. resolution . width)
                                 (videoSettings ^. resolution . height)
                                 Command.ForceOriginalAspectRatioDisable
                  )
                  []
                )
              )
            )
        , frameRate   = Just (videoSettings ^. frameRate)
        , mappings    = []
        , vcodec      = Just "h264"
        , acodec      = Just "aac"
        , format      = Just "mp4"
        }
    runFFmpegCommand (ProgressUpdate progressMsg) fullLength cmd
    return (TranscodedPath proxyPath)

newtype FFmpegVideoImportC m a = FFmpegVideoImportC { runFFmpegVideoImportC :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (MonadIO m, Carrier sig m) => Carrier (VideoImport :+: sig) (FFmpegVideoImportC m) where
  ret = pure
  eff = handleSum (FFmpegVideoImportC . eff . handleCoercible) $ \case
    Transcode settings original fullLength outDir k ->
      k (transcode' settings original fullLength outDir "Transcoding video file")
    GenerateVideoThumbnail original outDir k -> k =<< generateVideoThumbnail' original outDir
    ImportVideoFile classification vs defaultVideoSpeed srcFile outDir k ->
      case classification of
        Unclassified -> k $ do
          -- Copy asset to working directory
          assetPath <- liftIO $ do
            createDirectoryIfMissing True outDir
            let assetPath = outDir </> takeFileName srcFile
            copyFile srcFile assetPath
            return (OriginalPath assetPath)
          -- Generate thumbnail and return asset
          pure <$> filePathToVideoAsset vs defaultVideoSpeed outDir assetPath
        Classified   -> k $ do
          fullLength <- liftIO (getVideoFileDuration srcFile)
          divideProgress3
            (transcode' (vs ^. renderVideoSettings) original fullLength (outDir </> "transcoded") "Transcoding video to project settings")
            (\x -> (x,) <$> transcode' (vs ^. proxyVideoSettings) original fullLength (outDir </> "proxies") "Generating proxy video")
            (classifyScenes fullLength)
          where
            original = OriginalPath srcFile
            toSceneAsset (transcodedPath, proxyPath) fullLength n timeSpan = do
              let meta = AssetMetadata original fullLength
              return (VideoAsset meta transcodedPath proxyPath defaultVideoSpeed (Just (n, timeSpan)))
            classifyScenes fullLength paths@(_, proxyPath) =
              classifyMovement 1.0 (readVideoFile (proxyPath ^. unProxyPath) >-> toMassiv)
                & classifyMovingScenes fullLength
                & (>>= zipWithM (toSceneAsset paths fullLength) [1 ..])
    IsSupportedVideoFile p k ->
        -- TODO: actual check using FFmpeg
        k (takeExtension p `elem` [".mp4", ".m4v", ".webm", ".avi", ".mkv", ".mov", ".flv"])

runFFmpegVideoImport
  :: (MonadIO m, Carrier sig m) => Eff (FFmpegVideoImportC m) a -> m a
runFFmpegVideoImport = runFFmpegVideoImportC . interpret

data VideoImportError
  = UnexpectedError FilePath Text
  deriving (Show, Eq)

instance Exception VideoImportError
