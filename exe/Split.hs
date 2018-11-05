{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
module Main where

import           Komposition.Prelude

import           Codec.FFmpeg                    hiding (resolution)
import           Codec.FFmpeg.Encode
import           Control.Lens
import           Data.Massiv.Array.IO            as A hiding (Image)
import           Pipes                           (Consumer', Producer, (>->))
import qualified Pipes
import qualified Pipes.Parse                     as Pipes
import qualified Pipes.Prelude                   as Pipes hiding (show)
import           Pipes.Safe
import           System.Directory
import           System.FilePath
import           System.IO                       (hFlush)
import           Text.Printf

import           Komposition.Import.Video
import           Komposition.Import.Video.FFmpeg
import           Komposition.VideoSettings

main :: IO ()
main = do
  initialize
  args <- getArgs
  case args of
    [minStillTime, input, output] ->
      case readDouble (toS minStillTime) of
        Just s -> split videoSettings s input output
        Nothing -> putStrLn ("Invalid MIN_STILL_TIME, must be a value in seconds." :: Text)
    _               -> putStrLn ("Usage: komposition-split MIN_STILL_TIME INPUT OUTPUT" :: Text)
  where
    videoSettings = VideoSettings 25 (Resolution 640 480)


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
  -> Producer (Classified (Timed JuicyFrame)) m ()
  -> Producer (Classified (Timed JuicyFrame)) m [FilePath]
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

yield' :: Monad m => b -> Pipes.StateT (Producer a m x) (Producer b m) ()
yield' = lift . Pipes.yield

draw' :: Monad m => Pipes.StateT (Producer a m x) (Producer b m) (Maybe a)
draw' = Pipes.hoist lift Pipes.draw
