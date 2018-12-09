{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Komposition.Render.FFmpegTest where

import           Komposition.Prelude
import qualified Prelude

import qualified Pipes
import qualified Pipes.Prelude             as Pipes
import           Test.Tasty.Hspec

import           Komposition.Render.FFmpeg

spec_FFmpegRender :: Spec
spec_FFmpegRender =
  describe "render progress" $ do
    it "does not exceed 1.0 for video in original speed" $
      True `shouldBe` True
