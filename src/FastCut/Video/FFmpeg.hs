{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
module FastCut.Video.FFmpeg where

import           Codec.FFmpeg
import           Codec.FFmpeg.Encode
import           Codec.Picture
import           Codec.Picture.Types
import           Control.Monad.IO.Class
import           Control.Monad.Primitive
import           Data.Foldable
import           Data.Vector             (Vector)
import qualified Data.Vector.Generic     as Vector
import           Data.Word
import           Debug.Trace
import           Pipes                   (Consumer', Pipe, Producer)
import           Pipes                   as Pipes
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

writeVideoFile :: MonadIO m => FilePath -> Producer Frame m () -> m ()
writeVideoFile path source = do
  let ep = defaultH264 1920 1080
  writeFrame <- liftIO (imageWriter ep path)
  Pipes.runEffect $ Pipes.for source (liftIO . writeFrame . Just)
  liftIO (writeFrame Nothing)

dropTime :: Monad m => Pipe TimedFrame Frame m ()
dropTime = Pipes.map fst

equalFrame :: Frame -> Frame ->  Bool
equalFrame a b = Vector.and (Vector.zipWith (==) (imageData a) (imageData b))

data EqCount = EqCount Int Int

instance Semigroup EqCount where
  EqCount eq neq <> EqCount eq' neq' = EqCount (eq + eq') (neq + neq')

instance Monoid EqCount where
  mempty = EqCount 0 0

equalFrame2 :: Double -> Frame -> Frame ->  Bool
equalFrame2 eps a b = traceShow sim $ (wa, ha) == (wb, hb) && sim > (1 - eps)
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

equalFrameCountThreshold :: Int
equalFrameCountThreshold = 15

data MovementFrame
  = Moving Frame
  | Still Frame

unMovementFrame :: MovementFrame -> Frame
unMovementFrame = \case
  Moving f -> f
  Still f -> f

instance Eq MovementFrame where
  f1 == f2 = imageData (unMovementFrame f1) == imageData (unMovementFrame f2)

instance Show MovementFrame where
  show mf =
    case mf of
      Moving _ -> "Moving <frame>"
      Still _  -> "Still <frame>"

data ClassifierState
  = InMoving { equalFrames       :: !(Vector Frame)
             }
  | InStill { stillFrame :: !Frame }

instance Show ClassifierState where
  show s =
    case s of
      InMoving {..} -> "InMoving " <> show (Vector.length equalFrames)
      InStill {}    -> "InStill"

classifyMovement :: Monad m => Pipe Frame MovementFrame m ()
classifyMovement = do
  frame <- await
  void $ go (InMoving (Vector.singleton frame))
  where
    go state = do
      frame <- await
      case state of
        InMoving {..}
          | equalFrame2 0.02 frame (Vector.head equalFrames) ->
            if Vector.length equalFrames >= equalFrameCountThreshold
              then do
                Vector.mapM_ (yield . Still) equalFrames
                yield (Still frame)
                go (InStill frame)
              else go (InMoving (Vector.snoc equalFrames frame))
          | otherwise -> do
            Vector.mapM_ (yield . Moving) equalFrames
            go (InMoving (Vector.singleton frame))
        InStill {..}
          | equalFrame2 0.02 stillFrame frame -> do
              yield (Still frame)
              go (InStill stillFrame)
          | otherwise -> go (InMoving (Vector.singleton frame))

colorClassifiedMovement :: (PrimMonad m, MonadIO m) => Pipe MovementFrame Frame m ()
colorClassifiedMovement =
  Pipes.mapM $ \case
    Moving frame -> tint' green frame
    Still frame -> tint' red frame
  where
    tint' color frame = do
      mf <- thawImage frame
      tint color mf
      freezeImage mf
    red = PixelRGB8 255 0 0
    green = PixelRGB8 0 255 0

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

-- data SplitterState
--   = Initial
--   | InPart { partNumber      :: !Int
--            , writeToPart     :: Maybe Frame -> IO ()
--            , lastFrame       :: !Frame
--            , equalFrameCount :: !Int }
--   | InSplit { partNumber :: !Int
--             , splitFrame :: !Frame }
--
--
-- splitReader :: IO (Maybe (Image PixelRGB8)) -> IO a -> FilePath -> IO a
-- splitReader getFrame cleanup outDir = loop Initial
--   where
--     loop Initial = do
--       mf <- getFrame
--       case mf of
--         Just f  -> do
--           w <- partWriter outDir 0
--           loop (InPart 0 w f 1)
--         Nothing -> cleanup
--     loop InPart {..} = do
--       maybeNew <- getFrame
--       case maybeNew of
--         Just newFrame
--           | equalFrame2 32 newFrame lastFrame ->
--             if succ equalFrameCount > equalFrameCountThreshold
--             then do writeToPart Nothing
--                     loop (InSplit partNumber newFrame )
--             else do writeToPart (Just newFrame)
--                     loop (InPart partNumber writeToPart newFrame (succ equalFrameCount))
--           | otherwise -> do
--             writeToPart (Just newFrame)
--             loop (InPart partNumber writeToPart newFrame 1)
--         Nothing -> do
--           writeToPart Nothing
--           cleanup
--     loop InSplit {..} = do
--       maybeNew <- getFrame
--       case maybeNew of
--         Just newFrame
--           | equalFrame2 32 splitFrame newFrame ->
--             loop (InSplit partNumber splitFrame)
--           | otherwise -> do
--             w <- partWriter outDir (succ partNumber)
--             loop (InPart (succ partNumber) w newFrame 1)
--         Nothing ->
--           cleanup
--
-- partWriter :: FilePath -> Int -> IO (Maybe Frame -> IO ())
-- partWriter outDir n =
--   let ep = defaultH264 1920 1080
--   in imageWriter ep (outDir </> Prelude.show n <> ".mp4")

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
    readVideoFile src
    -- >-> Pipes.tee printProcessingInfo
    >-> dropTime
    >-> classifyMovement
    >-> colorClassifiedMovement
