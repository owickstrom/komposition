{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module FastCut.Video.FFmpegTest where

import           Codec.Picture
import           Pipes
import qualified Pipes.Prelude        as Pipes
import           Test.Tasty.Hspec

import           FastCut.Video.FFmpeg

colorImage c =
  generateImage (\_ _ -> c) 640 480

red = PixelRGB8 255 0 0
green = PixelRGB8 0 255 0

f1, f2 :: Frame
f1 = colorImage red
f2 = colorImage green

shouldClassifyAs inFrames outFrames =
  Pipes.toList (classifyMovement (Pipes.each inFrames)) `shouldBe` outFrames

spec_classifyMovement = do
  it "discards too short still section" $
    concat [[f1], replicate 10 f2, [f1]] `shouldClassifyAs`
    (Moving f1 : replicate 10 (Moving f2) ++ [Moving f1])
  it "classifies a still section" $
    concat [[f1], replicate 20 f2, [f1]] `shouldClassifyAs`
    concat [[Moving f1], replicate 20 (Still f2), [Moving f1]]
