{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}
module FastCut.Video.FFmpeg where

import           Codec.FFmpeg
import           Codec.FFmpeg.Encode
import           Codec.Picture           as CP
import           Codec.Picture.Types     as CP
import           Control.Monad.IO.Class
import           Control.Monad.Primitive
import           Data.Massiv.Array       as A
import           Data.Massiv.Array.IO    as A hiding (Image)
import           Data.Maybe              (fromMaybe)
import qualified Data.Vector             as V
import qualified Data.Vector.Generic     as VG
import           Data.Word
import           Graphics.ColorSpace     as A
import           Pipes                   (Consumer', Pipe, Producer)
import           Pipes                   as Pipes
import           Pipes.Parse
import qualified Pipes.Prelude           as Pipes
import           System.Directory
import           System.FilePath
import           System.IO
import           Text.Printf

initialize :: IO ()
initialize = initFFmpeg

type Frame = Image PixelRGB8
type TimedFrame = (Frame, Double)

readVideoFile :: MonadIO m => FilePath -> Producer TimedFrame m ()
readVideoFile path = do
  (getFrame, cleanup) <- liftIO (imageReaderTime (File path))
  yieldNext getFrame cleanup
  where
    yieldNext ::
         MonadIO m
      => IO (Maybe TimedFrame)
      -> IO end
      -> Producer TimedFrame m end
    yieldNext getFrame cleanup = do
      m <- liftIO getFrame
      case m of
        Just f -> do
          Pipes.yield f
          yieldNext getFrame cleanup
        Nothing -> liftIO cleanup

type RGB8Frame = A.Array A.S A.Ix2 (A.Pixel RGB Word8)

toMassiv :: MonadIO m => Pipe Frame RGB8Frame m ()
toMassiv =
  Pipes.map
    (A.setComp A.Seq .
     fromMaybe (error "Could not convert image") .
     A.fromDynamicImage . CP.ImageRGB8)

fromMassiv :: MonadIO m => Pipe RGB8Frame Frame m ()
fromMassiv = Pipes.map A.toJPImageRGB8


writeVideoFile :: MonadIO m => FilePath -> Producer Frame m () -> m ()
writeVideoFile path source = do
  let ep = defaultH264 1920 1080
  writeFrame <- liftIO (imageWriter ep path)
  Pipes.runEffect $ Pipes.for source (liftIO . writeFrame . Just)
  liftIO (writeFrame Nothing)

dropTime :: Monad m => Pipe TimedFrame Frame m ()
dropTime = Pipes.map fst

data EqCount = EqCount Int Int

instance Semigroup EqCount where
  EqCount eq neq <> EqCount eq' neq' = EqCount (eq + eq') (neq + neq')

instance Monoid EqCount where
  mempty = EqCount 0 0

equalFrame2 :: Double -> Frame -> Frame ->  Bool
equalFrame2 eps a b = (wa, ha) == (wb, hb) && sim > (1 - eps)
  where
    wa = imageWidth a
    ha = imageHeight a
    wb = imageWidth b
    hb = imageHeight b
    go (x, y) =
      if pixelAt a x y == pixelAt b x y
        then EqCount 1 0
        else EqCount 0 1
    sim :: Double
    sim =
      let (EqCount eq neq) =
            foldMap go [(x, y) | x <- [0 .. pred wa], y <- [0 .. pred ha]]
      in fromIntegral eq / fromIntegral (eq + neq)

equalFrame3 :: Word8 -> Double -> RGB8Frame -> RGB8Frame ->  Bool
equalFrame3 eps minEqPct f1 f2 =
  pct > minEqPct
  where
    cmp :: A.Pixel RGB Word8 -> A.Pixel RGB Word8 -> Int
    cmp px1 px2 = if A.eqTolPx eps px1 px2 then 1 else 0
    {-# INLINE cmp #-}
    sumEq = A.sum (A.zipWith cmp f1 f2)
    total = A.totalElem (A.size f1)
    pct = fromIntegral sumEq / fromIntegral total

equalFrameCountThreshold :: Int
equalFrameCountThreshold = 8

data MovementFrame
  = Moving RGB8Frame
  | Still RGB8Frame
  deriving (Eq)

unMovementFrame :: MovementFrame -> RGB8Frame
unMovementFrame = \case
  Moving f -> f
  Still f -> f

instance Show MovementFrame where
  show mf =
    case mf of
      Moving _ -> "Moving <frame>"
      Still _  -> "Still <frame>"

data ClassifierState
  = InMoving { equalFrames       :: !(V.Vector RGB8Frame)
             }
  | InStill { stillFrame :: !RGB8Frame }

instance Show ClassifierState where
  show s =
    case s of
      InMoving {..} -> "InMoving " <> show (VG.length equalFrames)
      InStill {}    -> "InStill"

yield' :: Monad m => b -> StateT (Producer a m x) (Producer b m) ()
yield' = lift . yield

draw' :: Monad m => StateT (Producer a m x) (Producer b m) (Maybe a)
draw' = hoist lift draw

classifyMovement :: Monad m => Producer RGB8Frame m () -> Producer MovementFrame m ()
classifyMovement =
  evalStateT $
  draw' >>= \case
    Just frame -> go (InMoving (VG.singleton frame))
    Nothing -> pure ()
  where
    go ::
         Monad m
      => ClassifierState
      -> StateT (Producer RGB8Frame m ()) (Producer MovementFrame m) ()
    go state =
       (state,) <$> draw' >>= \case
        (InMoving {..}, Just frame)
          | equalFrame3 1 0.995 frame (VG.head equalFrames) ->
            if VG.length equalFrames >= equalFrameCountThreshold
              then do
                VG.mapM_ (yield' . Still) equalFrames
                yield' (Still frame)
                go (InStill frame)
              else go (InMoving (VG.snoc equalFrames frame))
          | otherwise -> do
            VG.mapM_ (yield' . Moving) equalFrames
            go (InMoving (VG.singleton frame))
        (InMoving {..}, Nothing) -> VG.mapM_ (yield' . Moving) equalFrames
        (InStill {..}, Just frame)
          | equalFrame3 1 0.995 stillFrame frame -> do
            yield' (Still frame)
            go (InStill stillFrame)
          | otherwise -> go (InMoving (VG.singleton frame))
        (InStill {..}, Nothing) -> pure ()

colorClassifiedMovement :: (PrimMonad m, MonadIO m) => Pipe MovementFrame RGB8Frame m ()
colorClassifiedMovement =
  Pipes.mapM $ \case
    Moving frame -> tint' green frame
    Still frame -> tint' red frame
  where
    tint' color frame =
      pure $ A.compute $ A.map (\px -> meanWord8 <$> color <*> px) frame
      -- liftIO $ do
      -- mf <- A.unsafeThaw frame
      -- flip A.imapP_ frame $ \ix px ->
      --   A.unsafeWrite mf ix (meanWord8 <$> color <*> px)
      -- A.unsafeFreeze A.Par mf
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

printProcessingInfo :: MonadIO m => Consumer' TimedFrame m ()
printProcessingInfo =
  Pipes.mapM_ $ \(_, n) ->
    liftIO $ do
      let s = floor n :: Int
      printf
        "Processing at %02d:%02d:%02d\n"
        (s `div` 3600)
        (s `div` 60)
        (s `mod` 60)
      hFlush stdout

split :: FilePath -> FilePath -> IO ()
split src outDir = do
  createDirectoryIfMissing True outDir
  writeVideoFile (outDir </> "debug.mp4") $
    classifyMovement (readVideoFile src >-> dropTime >-> toMassiv)
    -- >-> Pipes.tee printProcessingInfo
    --
   >-> colorClassifiedMovement
   >-> fromMassiv
