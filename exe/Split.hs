module Main where

import           FastCut.Video.FFmpeg
import           System.Environment

main :: IO ()
main = do
  initialize
  args <- getArgs
  case args of
    [input, output] -> split input output
    _               -> fail "Usage: fastcut-split INPUT OUTPUT"
