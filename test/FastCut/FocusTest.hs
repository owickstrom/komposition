{-# LANGUAGE OverloadedStrings #-}

module FastCut.FocusTest where

import           Data.Semigroup   ((<>))
import           Test.Tasty.Hspec

import           FastCut.Focus
import           FastCut.Sequence


video1    = VideoClip () (ClipMetadata "video-1" "/tmp/1.mp4" 4)
video2    = VideoClip () (ClipMetadata "video-2" "/tmp/2.mp4" 10)
audio1    = AudioClip () (ClipMetadata "audio-1" "/tmp/1.m4a" 5)
audio2    = AudioClip () (ClipMetadata "audio-2" "/tmp/2.m4a" 8)
audio3    = AudioClip () (ClipMetadata "audio-3" "/tmp/3.m4a" 5)
gap1     = VideoGap () 1
gap2     = VideoGap () 3

singleLevelSequence =
  Sequence () [Composition () [gap1, video1] [audio1], Composition () [gap2, video2] [audio2, audio3]]

twoLevelSequence =
  Sequence () [ Sequence () []
              , Sequence () [ Composition () [gap1, video1] [audio1]
                            , Composition () [gap2, video2] [audio2, audio3]
                            ]
              ]

spec_modifyFocus = do

  it "moves the focus left within a single-level sequence" $ do
    let before' = InSequenceFocus 1 Here
        after' = InSequenceFocus 0 Here
    modifyFocus singleLevelSequence before' FocusLeft `shouldBe` Just after'

  it "maintains focus when left move is out of bounds" $ do
    let before' = InSequenceFocus 0 Here
    modifyFocus singleLevelSequence before' FocusLeft `shouldBe` Nothing

  it "maintains focus when right move is out of bounds" $ do
    let before' = InSequenceFocus 1 Here
    modifyFocus singleLevelSequence before' FocusRight `shouldBe` Nothing

  it "moves the focus left within innermost sequence of multi-level sequence" $ do
    let before' = InSequenceFocus 1 (InSequenceFocus 1 Here)
        after' = InSequenceFocus 1 (InSequenceFocus 0 Here)
    modifyFocus twoLevelSequence before' FocusLeft `shouldBe` Just after'

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
