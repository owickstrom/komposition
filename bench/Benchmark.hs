{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Komposition.Prelude

import qualified Codec.Picture        as Juicy
import           Criterion.Main
import qualified Data.ByteString      as ByteString
import qualified Data.Massiv.Array.IO as Massiv

import           Komposition.Import.Video

testImageName :: Int -> FilePath
testImageName n = "bench/images/" <> show n <> ".png"

readJuicyTestImage :: Int -> IO (Juicy.Image Juicy.PixelRGB8)
readJuicyTestImage n = do
  result <- Juicy.decodePng <$> ByteString.readFile (testImageName n)
  case result of
    Left err                    -> panic (toS err)
    Right (Juicy.ImageRGB8 img) -> return img
    Right _                     -> panic "Unexpected image type."

readMassivTestImage :: Int -> IO RGB8Frame
readMassivTestImage = Massiv.readImage . testImageName

main :: IO ()
main = do
  -- jimg1 <- readJuicyTestImage 1
  -- jimg2 <- readJuicyTestImage 2
  himg1 <- readMassivTestImage 1
  himg2 <- readMassivTestImage 2
  defaultMain
    [ bgroup
        "fib"
        [ bench "equalFrame(1)" $ whnf (equalFrame 1  0.999 himg1) himg1
        , bench "!equalFrame(1)" $ whnf (equalFrame 1  0.999 himg1) himg2
        , bench "equalFrame'(32)" $ whnf (equalFrame' 1 0.99 himg1) himg1
        , bench "!equalFrame'(32)" $ whnf (equalFrame' 1  0.99 himg1) himg2
        ]
    ]
