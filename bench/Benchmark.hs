{-# LANGUAGE OverloadedStrings #-}
module Main where

import           FastCut.Prelude

import qualified Codec.Picture        as Juicy
import           Criterion.Main
import qualified Data.ByteString      as ByteString
import qualified Data.Massiv.Array.IO as Massiv

import           FastCut.Import.Video

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
  jimg1 <- readJuicyTestImage 1
  jimg2 <- readJuicyTestImage 2
  -- mimg1 <- readMassivTestImage 1
  -- mimg2 <- readMassivTestImage 2
  defaultMain
    [ bgroup
        "fib"
        [ bench "equalFrame(1)" $ whnf (equalFrame 1  0.999 jimg1) jimg1
        , bench "!equalFrame(1)" $ whnf (equalFrame 1  0.999 jimg1) jimg2
        , bench "equalFrame(32)" $ whnf (equalFrame 32 0.999 jimg1) jimg1
        , bench "!equalFrame(32)" $ whnf (equalFrame 32  0.999 jimg1) jimg2
        ]
    ]
