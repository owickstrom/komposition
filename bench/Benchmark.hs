module Main where

import qualified Codec.Picture        as Juicy
import           Criterion.Main
import qualified Data.ByteString      as ByteString
import qualified Data.Massiv.Array.IO as Massiv

import           FastCut.Video.FFmpeg

testImageName :: Int -> FilePath
testImageName n = "bench/images/" <> show n <> ".png"

readJuicyTestImage :: Int -> IO (Juicy.Image Juicy.PixelRGB8)
readJuicyTestImage n = do
  result <- Juicy.decodePng <$> ByteString.readFile (testImageName n)
  case result of
    Left err                    -> fail err
    Right (Juicy.ImageRGB8 img) -> return img
    Right _                     -> fail "Unexpected image type."

readMassivTestImage :: Int -> IO RGB8Frame
readMassivTestImage = Massiv.readImage . testImageName

main :: IO ()
main = do
  jimg1 <- readJuicyTestImage 1
  jimg2 <- readJuicyTestImage 2
  mimg1 <- readMassivTestImage 1
  mimg2 <- readMassivTestImage 2
  defaultMain
    [ bgroup
        "fib"
        [ bench "equalFrame2(1)" $ whnf (equalFrame2 1 jimg1) jimg1
        , bench "!equalFrame2(1)" $ whnf (equalFrame2 1 jimg1) jimg2
        , bench "equalFrame2(32)" $ whnf (equalFrame2 32 jimg1) jimg1
        , bench "!equalFrame2(32)" $ whnf (equalFrame2 32 jimg1) jimg2
        , bench "equalFrame3(1, 0.995)" $ whnf (equalFrame3 1 0.995 mimg1) mimg1
        , bench "!equalFrame3(1, 0.995)" $
          whnf (equalFrame3 1 0.995 mimg1) mimg2
        ]
    ]
