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
module FastCut.Import.FFmpeg where

import           FastCut.Prelude         hiding (catch)

import           Codec.FFmpeg
import           Codec.FFmpeg.Encode
import qualified Codec.FFmpeg.Probe      as Probe
import           Codec.Picture           as CP
import           Codec.Picture.Types     as CP
import           Control.Monad.Catch
import           Control.Monad.Primitive
import qualified Data.Massiv.Array       as A
import           Data.Massiv.Array.IO    as A hiding (Image)
import           Data.Maybe              (fromMaybe)
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
import           Text.Printf

import           FastCut.Duration
import           FastCut.Library
import           FastCut.MediaType
import           FastCut.Progress

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
  } deriving (Functor)

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


writeVideoFile :: MonadIO m => FilePath -> Producer Frame m () -> m ()
writeVideoFile filePath source = do
  let ep = (defaultH264 800 450) { epFps = 25 }
  writeFrame <- liftIO (imageWriter ep filePath)
  Pipes.runEffect $ Pipes.for source (liftIO . writeFrame . Just)
  liftIO (writeFrame Nothing)

dropTime :: Monad m => Pipe (Timed Frame) Frame m ()
dropTime = Pipes.map untimed

data EqCount = EqCount Int Int

instance Semigroup EqCount where
  EqCount eq neq <> EqCount eq' neq' = EqCount (eq + eq') (neq + neq')

instance Monoid EqCount where
  mempty = EqCount 0 0

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

-- | Time (in seconds) of equal frames before going into 'Still'.
equalFrameTimeThreshold :: Time
equalFrameTimeThreshold = 0.5

data Classified f
  = Moving f
  | Still f
  deriving (Eq, Functor)

unClassified :: Classified f -> f
unClassified = \case
  Moving f -> f
  Still f -> f

data ClassifierState
  = InMoving { equalFrames :: !(V.Vector (Timed RGB8Frame)) }
  | InStill { stillFrame     :: !(Timed RGB8Frame)
            , nonEqualFrames :: !(V.Vector (Timed RGB8Frame)) }

yield' :: Monad m => b -> Pipes.StateT (Producer a m x) (Producer b m) ()
yield' = lift . Pipes.yield

draw' :: Monad m => Pipes.StateT (Producer a m x) (Producer b m) (Maybe a)
draw' = Pipes.hoist lift Pipes.draw

classifyMovement :: Monad m => Producer (Timed RGB8Frame) m () -> Producer (Classified (Timed RGB8Frame)) m ()
classifyMovement =
  Pipes.evalStateT $
  draw' >>= \case
    Just frame -> go (InMoving (VG.singleton frame))
    Nothing -> pure ()
  where
    go ::
         Monad m
      => ClassifierState
      -> Pipes.StateT (Producer (Timed RGB8Frame) m ()) (Producer (Classified (Timed RGB8Frame)) m) ()
    go state' =
       (state',) <$> draw' >>= \case
        (InMoving {..}, Just frame)
          | equalFrame 1 0.999 (untimed frame) (untimed (VG.head equalFrames)) ->
            if time frame - time (VG.head equalFrames) >= equalFrameTimeThreshold
              then do
                VG.mapM_ (yield' . Still) equalFrames
                yield' (Still frame)
                go (InStill frame VG.empty)
              else go (InMoving (VG.snoc equalFrames frame))
          | otherwise -> do
            VG.mapM_ (yield' . Moving) equalFrames
            go (InMoving (VG.singleton frame))
        (InMoving {..}, Nothing) -> VG.mapM_ (yield' . Moving) equalFrames
        (InStill {..}, Just frame)
          | equalFrame 1 0.999 (untimed stillFrame) (untimed frame) -> do
            yield' (Still frame)
            go (InStill stillFrame VG.empty)
          | otherwise -> go (InMoving (VG.singleton frame))
        (InStill {..}, Nothing) -> pure ()

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

split :: FilePath -> FilePath -> IO ()
split src outDir = do
  createDirectoryIfMissing True outDir
  res <- classifyMovement (readVideoFile src >-> toMassiv)
      >-> Pipes.tee (Pipes.map unClassified >-> printProcessingInfo)
      >-> colorClassifiedMovement
      >-> fromMassiv
      >-> dropTime
    & writeVideoFile (outDir </> "debug.mp4")
    & runExceptT
  putStrLn ("" :: Text)
  case res of
    Left err -> putStrLn ("Failed to split video file: " <> show err :: Text)
    Right () -> return ()

writeSplitVideoFiles ::
     MonadIO m
  => FilePath
  -> Producer (Classified (Timed Frame)) m ()
  -> Producer (Classified (Timed Frame)) m [FilePath]
writeSplitVideoFiles outDir = Pipes.evalStateT (go (Left (), 1 :: Int, []))
  where
    go state' =
      draw' >>= \case
        Just frame -> do
          yield' frame
          case (state', frame) of
            ((Left (), n, files), Still _) -> go (Left (), n, files)
            ((Left (), n, files), Moving f) -> do
              let ep = (defaultH264 800 450) {epFps = 25}
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

getVideoFileDuration :: (MonadMask m, MonadIO m) => FilePath -> m Duration
getVideoFileDuration f =
  Duration . picosecondsToDiffTime . (* 1000000) . fromIntegral <$>
  Probe.withAvFile f Probe.duration

filePathToVideoAsset ::
     (MonadError VideoImportError m, MonadIO m)
  => FilePath
  -> FilePath
  -> m (Asset Video)
filePathToVideoAsset outDir p = do
  md <-
    AssetMetadata p
    <$> liftIO (getVideoFileDuration p)
    <*> generateVideoThumbnail p outDir
  pure (VideoAsset md)

generateVideoThumbnail ::
     (MonadError VideoImportError m, MonadIO m)
  => FilePath
  -> FilePath
  -> m FilePath
generateVideoThumbnail sourceFile outDir = do
  (readImage', cleanup) <- liftIO (imageReader (File sourceFile))
  liftIO readImage' >>= \case
    Just (img :: Image PixelRGB8) -> do
      let fileName = outDir </> replaceExtension (snd (splitFileName sourceFile)) "png" <> ""
      liftIO (writePng fileName img)
      liftIO cleanup
      pure fileName
    Nothing -> do
      liftIO cleanup
      throwError (UnexpectedError sourceFile "No frames in video.")

newtype AutoSplit = AutoSplit Bool deriving (Show, Eq)

data VideoImportError
  = UnexpectedError FilePath Text
  deriving (Show, Eq)

importVideoFile ::
     MonadIO m
  => FilePath
  -> FilePath
  -> Producer ProgressUpdate m (Either VideoImportError (Asset Video))
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

importVideoFileAutoSplit ::
     MonadIO m
  => FilePath
  -> FilePath
  -> Producer ProgressUpdate m (Either VideoImportError [Asset Video])
importVideoFileAutoSplit sourceFile outDir = do
  liftIO (createDirectoryIfMissing True outDir)
  fullLength <- liftIO (getVideoFileDuration sourceFile)
  let classifiedFrames =
        classifyMovement (readVideoFile sourceFile >-> toMassiv) >->
        Pipes.map (fmap (fmap A.toJPImageRGB8))
  writeSplitVideoFiles outDir classifiedFrames >->
    Pipes.map (toProgress fullLength . unClassified) &
    (>>= mapM (filePathToVideoAsset outDir)) &
    Pipes.runExceptP
  where
    toProgress (durationToSeconds -> fullLength) Timed {..} =
      ProgressUpdate (time / fullLength)
