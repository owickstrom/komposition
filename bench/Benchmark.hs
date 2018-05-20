module Main where

import Control.Lens.Traversal
import           Codec.Picture
import           Codec.Picture.Types
import           Codec.Picture.Png
import           Criterion.Main
import qualified Data.ByteString      as ByteString
import           Data.Vector.Storable          (Vector)
import qualified Data.Vector.Storable          as Vector

import           FastCut.Video.FFmpeg

readTestImage :: Int -> IO (Image PixelRGB8)
readTestImage n = do
  let name = "bench/images/" <> show n <> ".png"
  result <- decodePng <$> ByteString.readFile name
  case result of
    Left err              -> fail err
    Right (ImageRGB8 img) -> return img
    Right _               -> fail "Unexpected image type."

equalFrame' (img1, img2) =
  equalFrame img1 img2

main = do
  img1 <- readTestImage 1
  img2 <- readTestImage 2

  defaultMain
    [ bgroup
        "fib"
        [ bench "equal" $ whnf equalFrame' (img1, img1)
        , bench "nonequal" $ whnf equalFrame' (img1, img2)
        ]
    ]
