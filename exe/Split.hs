module Main where

import           FastCut.Prelude

import           FastCut.Import.Video

main :: IO ()
main = do
  initialize
  args <- getArgs
  case args of
    [minStillTime, input, output] ->
      case readDouble (toS minStillTime) of
        Just s -> split s input output
        Nothing -> putStrLn "Invalid MIN_STILL_TIME, must be a value in seconds."
    _               -> putStrLn "Usage: fastcut-split MIN_STILL_TIME INPUT OUTPUT"
