module Main where

import           Komposition.Prelude

import           Komposition.Import.Video
import           Komposition.VideoSettings

main :: IO ()
main = do
  initialize
  args <- getArgs
  case args of
    [minStillTime, input, output] ->
      case readDouble (toS minStillTime) of
        Just s -> split videoSettings s input output
        Nothing -> putStrLn "Invalid MIN_STILL_TIME, must be a value in seconds."
    _               -> putStrLn "Usage: komposition-split MIN_STILL_TIME INPUT OUTPUT"

  where
    videoSettings = VideoSettings 25 (Resolution 640 480)
