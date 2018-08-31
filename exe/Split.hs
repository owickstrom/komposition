module Main where

import           FastCut.Prelude

import           FastCut.Import.Video

main :: IO ()
main = do
  initialize
  args <- getArgs
  case args of
    [input, output] -> split input output
    _               -> putStrLn "Usage: fastcut-split INPUT OUTPUT"
