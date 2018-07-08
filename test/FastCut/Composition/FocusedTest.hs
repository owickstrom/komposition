{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module FastCut.Composition.FocusedTest where

import           FastCut.Prelude
import qualified Prelude

import           Test.Tasty.Hspec

import           FastCut.Composition
import           FastCut.Composition.Focused
import           FastCut.Focus


video4s = Clip $ VideoClip () (ClipMetadata "video-1" "/tmp/1.mp4" 4)
video10s = Clip $ VideoClip () (ClipMetadata "video-2" "/tmp/2.mp4" 10)
audio1s = Clip $ AudioClip () (ClipMetadata "audio-1" "/tmp/1.m4a" 1)
audio4s = Clip $ AudioClip () (ClipMetadata "audio-2" "/tmp/2.m4a" 4)
audio10s = Clip $ AudioClip () (ClipMetadata "audio-3" "/tmp/3.m4a" 10)
gap1s = Gap () 1
gap3s = Gap () 3
parallel1 = Parallel () [gap1s, video4s] [audio1s]
parallel2 = Parallel () [gap3s, video10s] [audio4s, audio10s]

seqWithTwoParallels = Sequence () [parallel1, parallel2]

timelineTwoParallels = Timeline () [Sequence () [], seqWithTwoParallels]

spec_applyFocus = do
  it "applies focus to sequence" $
    applyFocus
      (Timeline () [Sequence () [Parallel () [Gap () 1] []]])
      (SequenceFocus 0 Nothing) `shouldBe`
    Timeline
      TransitivelyFocused
      [Sequence Focused [Parallel Blurred [Gap Blurred 1] []]]
  it "applies focus to parallel" $
    applyFocus
      (Timeline () [Sequence () [Parallel () [Gap () 1] []]])
      (SequenceFocus 0 (Just (ParallelFocus 0 Nothing))) `shouldBe`
    Timeline
      TransitivelyFocused
      [Sequence TransitivelyFocused [Parallel Focused [Gap Blurred 1] []]]

{-# ANN module ("HLint: ignore Use camelCase" :: Prelude.String) #-}
