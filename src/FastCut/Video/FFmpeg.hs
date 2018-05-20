{-# LANGUAGE RecordWildCards #-}
module FastCut.Video.FFmpeg where

import           Codec.FFmpeg
import           Codec.FFmpeg.Encode
import           Codec.Picture
import           Control.DeepSeq
import           Control.Monad
import           Control.Parallel.Strategies
import           Data.Vector.Storable        (Vector)
import qualified Data.Vector.Storable        as Vector
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
  | InPart { partNumber      :: Int
           , writeToPart     :: Maybe Frame -> IO ()
           , lastFrame       :: Frame
           , equalFrameCount :: Int }
  | InSplit { partNumber :: Int
            , splitFrame :: Frame }

parVector :: (NFData a, Vector.Storable a) => Strategy (Vector a)
parVector vec =
  let vLen = Vector.length vec
      half = vLen `div` 2
      minChunk = 1024
  in  if vLen > minChunk
      then do
        let v1 = Vector.unsafeSlice 0 half vec
            v2 = Vector.unsafeSlice half (vLen - half) vec
        void (parVector v1)
        void (parVector v2)
        return vec
      else
        evalChunk (vLen-1) >>
        return vec
  where
  evalChunk 0 = rpar (rdeepseq (vec Vector.! 0)) >> return vec
  evalChunk i = rpar (rdeepseq (vec Vector.! i)) >> evalChunk (i-1)

equalFrame :: Frame -> Frame ->  Bool
equalFrame a b =
  Vector.and (Vector.zipWith (==) (imageData a) (imageData b))

equalFrameCountThreshold :: Int
equalFrameCountThreshold = 25

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
          | equalFrame newFrame lastFrame ->
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
          | imageData splitFrame == imageData newFrame ->
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
