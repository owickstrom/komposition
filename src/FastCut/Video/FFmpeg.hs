{-# LANGUAGE RecordWildCards #-}
module FastCut.Video.FFmpeg where

import           Codec.FFmpeg
import           Codec.FFmpeg.Encode
import           Codec.Picture
import qualified Data.Vector.Storable as Vector
import           System.Directory
import           System.FilePath

initialize :: IO ()
initialize = initFFmpeg

split :: FilePath -> FilePath -> IO ()
split from outDir = do
  (getFrame, cleanup) <- imageReader (File from)
  createDirectoryIfMissing True outDir
  splitReader getFrame cleanup outDir

type Frame = Image PixelRGB8

data SplitterState
  = Initial
  | InPart { partNumber      :: !Int
           , writeToPart     :: Maybe Frame -> IO ()
           , lastFrame       :: !Frame
           , equalFrameCount :: !Int }
  | InSplit { partNumber :: !Int
            , splitFrame :: !Frame }

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

splitReader :: IO (Maybe (Image PixelRGB8)) -> IO a -> FilePath -> IO a
splitReader getFrame cleanup outDir = loop Initial
  where
    loop Initial = do
      mf <- getFrame
      case mf of
        Just f  -> do
          w <- partWriter outDir 0
          loop (InPart 0 w f 1)
        Nothing -> cleanup
    loop InPart {..} = do
      maybeNew <- getFrame
      case maybeNew of
        Just newFrame
          | equalFrame2 32 newFrame lastFrame ->
            if succ equalFrameCount > equalFrameCountThreshold
            then do writeToPart Nothing
                    loop (InSplit partNumber newFrame )
            else do writeToPart (Just newFrame)
                    loop (InPart partNumber writeToPart newFrame (succ equalFrameCount))
          | otherwise -> do
            writeToPart (Just newFrame)
            loop (InPart partNumber writeToPart newFrame 1)
        Nothing -> do
          writeToPart Nothing
          cleanup
    loop InSplit {..} = do
      maybeNew <- getFrame
      case maybeNew of
        Just newFrame
          | equalFrame2 32 splitFrame newFrame ->
            loop (InSplit partNumber splitFrame)
          | otherwise -> do
            w <- partWriter outDir (succ partNumber)
            loop (InPart (succ partNumber) w newFrame 1)
        Nothing ->
          cleanup

partWriter :: FilePath -> Int -> IO (Maybe Frame -> IO ())
partWriter outDir n =
  let ep = defaultH264 1920 1080
  in imageWriter ep (outDir </> show n <> ".mp4")
