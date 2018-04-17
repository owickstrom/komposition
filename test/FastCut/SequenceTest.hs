{-# LANGUAGE OverloadedStrings #-}

module FastCut.SequenceTest where

import           Data.Semigroup   ((<>))
import           Test.Tasty.Hspec

import           FastCut.Sequence

video1 = VideoClip () (ClipMetadata "video-1" "/tmp/1.mp4" 1)
video2 = VideoClip () (ClipMetadata "video-2" "/tmp/2.mp4" 10)
audio1 = AudioClip () (ClipMetadata "audio-1" "/tmp/1.m4a" 2)
audio2 = AudioClip () (ClipMetadata "audio-2" "/tmp/2.m4a" 11)

spec_semigroup_concatenation = it "joins two sequences" $ do
  let videoAndAudio1 = Sequence () [single video1, single audio1]
      videoAndAudio2 = Sequence () [single video2, single audio2]
  videoAndAudio1 <> videoAndAudio2
  `shouldBe`
  Sequence () [single video1, single audio1, single video2, single audio2]

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
