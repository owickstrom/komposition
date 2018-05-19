{-# LANGUAGE RecordWildCards #-}
module FastCut.Video.FFmpeg where

import           Codec.FFmpeg
import           Codec.FFmpeg.Encode
import           Codec.Picture
import           Control.Monad
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
  | InPart { partNumber :: Int
           , frames     :: [Frame]
           }
  | InSplit { partNumber  :: Int
            , partFrames  :: [Frame]
            , splitFrames :: [Frame]
            }

enoughEqualFrames :: Frame -> [Frame] ->  Bool
enoughEqualFrames reference =
  all (\f -> imageData f == imageData reference) . take 5

splitReader :: IO (Maybe (Image PixelRGB8)) -> IO a -> FilePath -> IO a
splitReader getFrame cleanup outDir = loop Initial
  where
    loop Initial = do
      mf <- getFrame
      case mf of
        Just f  -> loop (InPart 0 [f])
        Nothing -> cleanup
    loop InPart {..} = do
      maybeNew <- getFrame
      case maybeNew of
        Just newFrame
          | enoughEqualFrames newFrame frames ->
            loop (InSplit partNumber frames [newFrame])
          | otherwise ->
            loop (InPart partNumber (newFrame : frames))
        Nothing -> do
          writePart partNumber frames []
          cleanup
    loop InSplit {..} = do
      maybeNew <- getFrame
      case maybeNew of
        Just newFrame
          | imageData (head partFrames) == imageData newFrame ->
            loop (InSplit partNumber partFrames (newFrame : splitFrames))
          | otherwise -> do
            writePart partNumber partFrames splitFrames
            loop (InPart (succ partNumber) [newFrame])
        Nothing -> cleanup
    writePart :: Int -> [Frame] -> [Frame] -> IO ()
    writePart n partFrames splitFrames = do
      let ep = defaultH264 1920 1080
      w <- imageWriter ep (outDir </> show n <> ".mp4")
      let writeAll = mapM_ (w . Just) . reverse
      writeAll partFrames
      when (length splitFrames < 5) $
        writeAll splitFrames
      w Nothing
