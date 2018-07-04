{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module FastCut.FocusTest where

import           Test.Tasty.Hspec

import           FastCut.Focus
import           FastCut.Sequence


video4s = Clip $ VideoClip () (ClipMetadata "video-1" "/tmp/1.mp4" 4)
video10s = Clip $ VideoClip () (ClipMetadata "video-2" "/tmp/2.mp4" 10)
audio1s = Clip $ AudioClip () (ClipMetadata "audio-1" "/tmp/1.m4a" 1)
audio4s = Clip $ AudioClip () (ClipMetadata "audio-2" "/tmp/2.m4a" 4)
audio10s = Clip $ AudioClip () (ClipMetadata "audio-3" "/tmp/3.m4a" 10)
gap1s = Gap () 1
gap3s = Gap () 3
composition1 = Composition () [gap1s, video4s] [audio1s]
composition2 = Composition () [gap3s, video10s] [audio4s, audio10s]

singleLevelSequence = Sequence
  ()
  [ Composition () [gap1s, video4s] [audio1s]
  , Composition () [gap3s, video10s] [audio4s, audio10s]
  ]

twoLevelSequence =
  Sequence () [Sequence () [], Sequence () [composition1, composition2]]

spec_applyFocus = do

  it "applies focus to first-level sequence"
    $          applyFocus (Sequence () [Sequence () [Composition () [Gap () 1] []]])
                          (SubFocus 0 SequenceFocus)
    `shouldBe` Sequence TransitivelyFocused [Sequence Focused [Composition Blurred [Gap Blurred 1] []]]

  it "applies focus to second-level composition"
    $          applyFocus (Sequence () [Sequence () [Composition () [Gap () 1] []]])
                          (SubFocus 0 (  (SubFocus 0 SequenceFocus)))
    `shouldBe` Sequence TransitivelyFocused [Sequence TransitivelyFocused [Composition Focused [Gap Blurred 1] []]]

spec_modifyFocus = do

  it "moves the focus left within a single-level sequence" $ do
    let before' = SubFocus 1 SequenceFocus
        after'  = SubFocus 0 SequenceFocus
    modifyFocus singleLevelSequence FocusLeft before' `shouldBe` pure after'

  it "maintains sequence focus when left move is out of bounds" $ do
    let before' = SubFocus 0 SequenceFocus
    modifyFocus singleLevelSequence FocusLeft before'
      `shouldBe` Left (OutOfBounds singleLevelSequence FocusLeft before')

  it "maintains sequence focus when right move is out of bounds" $ do
    let before' = SubFocus 1 SequenceFocus
    modifyFocus singleLevelSequence FocusRight before'
      `shouldBe` Left (OutOfBounds singleLevelSequence FocusRight before')

  it "moves the focus left within innermost sequence of multi-level sequence"
    $ do
        let before' = SubFocus 1 (SubFocus 1 SequenceFocus)
            after'  = SubFocus 1 (SubFocus 0 SequenceFocus)
        modifyFocus twoLevelSequence FocusLeft before' `shouldBe` pure after'

  it "maintains composition focus when left move is out of bounds" $ do
    let before' = SubFocus 0 (ClipFocus Video 0)
    modifyFocus singleLevelSequence FocusLeft before' `shouldBe` Left
      (OutOfBounds composition1 FocusLeft (ClipFocus Video 0))

  it "maintains composition focus when right move is out of bounds" $ do
    let before' = SubFocus 0 (ClipFocus Video 1)
    modifyFocus singleLevelSequence FocusRight before' `shouldBe` Left
      (OutOfBounds composition1 FocusRight (ClipFocus Video 1))

  it "moves the focus up from innermost sequence of multi-level sequence" $ do
    let before' = SubFocus 1 (SubFocus 1 SequenceFocus)
        after'  = SubFocus 1 SequenceFocus
    modifyFocus twoLevelSequence FocusUp before' `shouldBe` pure after'

  it "moves the focus up from video in multi-level sequence" $ do
    let before' = SubFocus
          1
          (SubFocus 1 (ClipFocus Video 1))
        after' = SubFocus 1 (SubFocus 1 SequenceFocus)
    modifyFocus twoLevelSequence FocusUp before' `shouldBe` pure after'

  it "moves the focus up from audio into video with nearest start to the left" $ do
    let sequence' = Sequence
          () [ Composition () [gap3s, video4s] [gap1s, gap3s, audio1s] ]
        before' = SubFocus 0 (ClipFocus Audio 2)
        after'  = SubFocus 0 (ClipFocus Video 1)
    modifyFocus sequence' FocusUp before' `shouldBe` pure after'

  it "moves the focus up from audio into video with same starting point" $ do
    let sequence' = Sequence () [ Composition () [gap3s, video10s] [gap3s, audio10s] ]
        before' = SubFocus 0 (ClipFocus Audio 1)
        after'  = SubFocus 0 (ClipFocus Video 1)
    modifyFocus sequence' FocusUp before' `shouldBe` pure after'

  it "maintains top sequence focus when trying to move up" $ do
    let before' = SubFocus 0 SequenceFocus
    modifyFocus twoLevelSequence FocusUp before' `shouldBe` Left CannotMoveUp

  it "moves the focus down into a composition in single-level sequence" $ do
    let before' = SubFocus 0 SequenceFocus
        after'  = SubFocus 0 (ClipFocus Video 0)
    modifyFocus singleLevelSequence FocusDown before' `shouldBe` pure after'

  it "moves the focus down from video into audio in composition" $ do
    let before' = SubFocus 0 (ClipFocus Video 0)
        after'  = SubFocus 0 (ClipFocus Audio 0)
    modifyFocus singleLevelSequence FocusDown before' `shouldBe` pure after'

  it "moves the focus down from video into audio with nearest start to the left" $ do
    let sequence' = Sequence
          () [ Composition () [gap3s, video4s] [gap1s, gap3s, audio1s] ]
        before' = SubFocus 0 (ClipFocus Video 1)
        after'  = SubFocus 0 (ClipFocus Audio 1)
    modifyFocus sequence' FocusDown before' `shouldBe` pure after'

  it "moves the focus up from audio into video with same starting point" $ do
    let sequence' = Sequence () [ Composition () [gap3s, video10s] [gap3s, audio10s] ]
        before' = SubFocus 0 (ClipFocus Video 1)
        after'  = SubFocus 0 (ClipFocus Audio 1)
    modifyFocus sequence' FocusDown before' `shouldBe` pure after'

spec_appendAt = do
  it "appends a group after the focused one" $ do
    let focus = SubFocus (0 :: Int) SequenceFocus
        before = Sequence 1 [ Sequence 2 [], Sequence 4 [] ]
        after = Sequence 1 [ Sequence 2 [], Sequence 3 [], Sequence 4 [] ]
    appendAt focus (Left (Sequence 3 [])) before `shouldBe` Just after

  it "appends a video clip after the focused one" $ do
    let focus = SubFocus 0 (ClipFocus Video 0)
        before = Sequence () [Composition () [video4s, video10s] []]
        after = Sequence () [Composition () [video4s, gap3s, video10s] []]
    appendAt focus (Right gap3s) before `shouldBe` Just after

  it "does not append at the top group" $ do
    let focus = SequenceFocus
        before = Sequence (0 :: Int) []
    appendAt focus (Left (Sequence 2 [])) before `shouldBe` Nothing

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
