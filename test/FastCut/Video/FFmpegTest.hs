{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module FastCut.Video.FFmpegTest where

import           Codec.Picture
import           Codec.Picture.Types
import           Pipes                as Pipes
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

spec_classifyMovement = do
  it "classifies a still section" $
    Pipes.toList (classifyMovement (Pipes.each [f1, f2, f2, f1])) `shouldBe`
    [Moving f1, Moving f2, Moving f2, Moving f1]
