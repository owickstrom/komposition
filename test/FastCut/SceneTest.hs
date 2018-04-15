{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module FastCut.SceneTest where

import           Test.Tasty.Hspec

import           FastCut.Scene

video1 = VideoClip (ClipMetadata "video-1" "/tmp/1.mp4" 1)
video2 = VideoClip (ClipMetadata "video-2" "/tmp/2.mp4" 10)
audio1 = AudioClip (ClipMetadata "audio-1" "/tmp/1.m4a" 2)
audio2 = AudioClip (ClipMetadata "audio-2" "/tmp/2.m4a" 11)

spec_semigroup_concatenation = it "joins two Sequenced lists" $ do
  let videoAndAudio1 = Sequenced [single video1, single audio1]
      videoAndAudio2 = Sequenced [single video2, single audio2]
  videoAndAudio1 <> videoAndAudio2
  `shouldBe`
  Sequenced [single video1, single audio1, single video2, single audio2]

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
