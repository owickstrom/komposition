{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
module FastCut.Video.FFmpeg where

import           Codec.FFmpeg
import           Codec.FFmpeg.Encode
import           Codec.Picture
import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Bits
import qualified Data.Vector.Storable   as Vector
import           Pipes                  (Consumer', Pipe, Producer)
import           Pipes                  as Pipes
import           Pipes.Extras           (scan1)
import qualified Pipes.Prelude          as Pipes
import           System.Directory
import           System.FilePath

initialize :: IO ()
initialize = initFFmpeg

type Frame = Image PixelRGB8

readVideoFile :: MonadIO m => FilePath -> Producer Frame m ()
readVideoFile path = do
  (getFrame, cleanup) <- liftIO (imageReader (File path))
  yieldNext getFrame cleanup
  where
    yieldNext getFrame cleanup = do
      m <- liftIO getFrame
      case m of
        Just f  -> do
          Pipes.yield f
          yieldNext getFrame cleanup
        Nothing -> liftIO cleanup

writeVideoFile :: MonadIO m => FilePath -> Producer Frame m () -> m ()
writeVideoFile path source = do
  let ep = defaultH264 1920 1080
  writeFrame <- liftIO (imageWriter ep path)
  Pipes.runEffect $ Pipes.for source (liftIO . writeFrame . Just)
  liftIO (writeFrame Nothing)

printFrames :: MonadIO m => Consumer' Frame m ()
printFrames = do
  f <- Pipes.await
  liftIO (Prelude.print (Vector.length (imageData f)))
  printFrames

equalFrame :: Frame -> Frame ->  Bool
equalFrame a b = Vector.and (Vector.zipWith (==) (imageData a) (imageData b))

equalFrame2 :: Int -> Frame -> Frame ->  Bool
equalFrame2 skip a b = la == lb && go 0
  where
    da = imageData a
    db = imageData b
    la = Vector.length da
    lb = Vector.length db
    go i
      | i >= la = True
      | otherwise = Vector.unsafeIndex da i == Vector.unsafeIndex db i && go (i + skip)

equalFrameCountThreshold :: Int
equalFrameCountThreshold = 60

data MovementFrame
  = Moving Frame
  | Still Frame

data ClassifierState
  = InMoving { lastFrame       :: !Frame
             , equalFrameCount :: !Int }
  | InStill { stillFrame :: !Frame }


classifyMovement :: MonadIO m => Pipe Frame MovementFrame m ()
classifyMovement = scan1 next first toMovementFrame
  where
    first frame = InMoving frame 1
    next state frame =
      case state of
        InMoving {..}
          | equalFrame2 32 frame lastFrame ->
            if succ equalFrameCount > equalFrameCountThreshold
              then InStill frame
              else InMoving frame (succ equalFrameCount)
          | otherwise -> InMoving frame 1
        InStill {..}
          | equalFrame2 32 stillFrame frame -> InStill stillFrame
          | otherwise -> InMoving frame 1
    toMovementFrame =
      \case
        InMoving {..} -> Moving lastFrame
        InStill {..} -> Still stillFrame

colorClassifiedMovement :: MonadIO m => Pipe MovementFrame Frame m ()
colorClassifiedMovement =
  Pipes.map $ \case
    Moving frame -> runIdentity (traverseOf imagePixels (tint 0x0000FF) frame)
    Still frame -> runIdentity (traverseOf imagePixels (tint 0xFF0000) frame)
  where
    tint color = pure . colorMap (.&. color)

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

split :: FilePath -> FilePath -> IO ()
split from outDir = do
  createDirectoryIfMissing True outDir
  writeVideoFile (outDir </> "colored.mp4") (readVideoFile from >-> classifyMovement >-> colorClassifiedMovement)
