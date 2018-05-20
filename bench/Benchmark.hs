module Main where

import           Codec.Picture
import           Criterion.Main
import qualified Data.ByteString      as ByteString

import           FastCut.Video.FFmpeg

readTestImage :: Int -> IO (Image PixelRGB8)
readTestImage n = do
  let name = "bench/images/" <> show n <> ".png"
  result <- decodePng <$> ByteString.readFile name
  case result of
    Left err              -> fail err
    Right (ImageRGB8 img) -> return img
    Right _               -> fail "Unexpected image type."

main :: IO ()
main = do
  img1 <- readTestImage 1
  img2 <- readTestImage 2

  defaultMain
    [ bgroup
        "fib"
        [ bench "equalFrame" $ whnf (equalFrame img1) img1
        , bench "!equalFrame" $ whnf (equalFrame img1) img2
        , bench "equalFrame2(1)" $ whnf (equalFrame2 1 img1) img1
        , bench "!equalFrame2(1)" $ whnf (equalFrame2 1 img1) img2
        , bench "equalFrame2(32)" $ whnf (equalFrame2 32 img1) img1
        , bench "!equalFrame2(32)" $ whnf (equalFrame2 32 img1) img2
        ]
    ]
