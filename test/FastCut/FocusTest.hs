{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module FastCut.FocusTest where

import           FastCut.Prelude
import qualified Prelude

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
parallel1 = Parallel () [gap1s, video4s] [audio1s]
parallel2 = Parallel () [gap3s, video10s] [audio4s, audio10s]

singleLevelSequence = Sequence
  ()
  [ Parallel () [gap1s, video4s] [audio1s]
  , Parallel () [gap3s, video10s] [audio4s, audio10s]
  ]

twoLevelSequence =
  Timeline () [Sequence () [], Sequence () [parallel1, parallel2]]

spec_applyFocus = do

  it "applies focus to first-level sequence"
    $          applyFocus (Timeline () [Sequence () [Parallel () [Gap () 1] []]])
                          (SubFocus 0 SequenceFocus)
    `shouldBe` Timeline TransitivelyFocused [Sequence Focused [Parallel Blurred [Gap Blurred 1] []]]

  it "applies focus to parallel"
    $          applyFocus (Timeline () [Sequence () [Parallel () [Gap () 1] []]])
                          (SubFocus 0 (  (SubFocus 0 SequenceFocus)))
    `shouldBe` Timeline TransitivelyFocused [Sequence TransitivelyFocused [Parallel Focused [Gap Blurred 1] []]]

spec_modifyFocus = do

  it "moves the focus left within a single-level sequence" $ do
    let before' = SubFocus 1 SequenceFocus
        after'  = SubFocus 0 SequenceFocus
    modifyFocus singleLevelSequence FocusLeft before' `shouldBe` pure after'

  it "maintains sequence focus when left move is out of bounds" $ do
    let before' = SubFocus 0 SequenceFocus
    modifyFocus singleLevelSequence FocusLeft before'
      `shouldBe` Left (OutOfBounds FocusLeft before')

  it "maintains sequence focus when right move is out of bounds" $ do
    let before' = SubFocus 1 SequenceFocus
    modifyFocus singleLevelSequence FocusRight before'
      `shouldBe` Left (OutOfBounds FocusRight before')

  it "moves the focus left within innermost sequence of multi-level sequence"
    $ do
        let before' = SubFocus 1 (SubFocus 1 SequenceFocus)
            after'  = SubFocus 1 (SubFocus 0 SequenceFocus)
        modifyFocus twoLevelSequence FocusLeft before' `shouldBe` pure after'

  it "maintains parallel focus when left move is out of bounds" $ do
    let before' = SubFocus 0 (ClipFocus Video 0)
    modifyFocus singleLevelSequence FocusLeft before' `shouldBe` Left
      (OutOfBounds FocusLeft (ClipFocus Video 0))

  it "maintains parallel focus when right move is out of bounds" $ do
    let before' = SubFocus 0 (ClipFocus Video 1)
    modifyFocus singleLevelSequence FocusRight before' `shouldBe` Left
      (OutOfBounds FocusRight (ClipFocus Video 1))

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
          () [ Parallel () [gap3s, video4s] [gap1s, gap3s, audio1s] ]
        before' = SubFocus 0 (ClipFocus Audio 2)
        after'  = SubFocus 0 (ClipFocus Video 1)
    modifyFocus sequence' FocusUp before' `shouldBe` pure after'

  it "moves the focus up from audio into video with same starting point" $ do
    let sequence' = Sequence () [ Parallel () [gap3s, video10s] [gap3s, audio10s] ]
        before' = SubFocus 0 (ClipFocus Audio 1)
        after'  = SubFocus 0 (ClipFocus Video 1)
    modifyFocus sequence' FocusUp before' `shouldBe` pure after'

  it "maintains top sequence focus when trying to move up" $ do
    let before' = SubFocus 0 SequenceFocus
    modifyFocus twoLevelSequence FocusUp before' `shouldBe` Left CannotMoveUp

  it "moves the focus down from sequence into a parallel" $ do
    let before' = SubFocus 0 SequenceFocus
        after'  = SubFocus 0 (SubFocus 0 SequenceFocus)
    modifyFocus twoLevelSequence FocusDown before' `shouldBe` pure after'

  it "cannot move the focus down into an empty sequence" $ do
    modifyFocus (Sequence () []) FocusDown SequenceFocus
      `shouldBe` Left CannotMoveDown

  it "cannot move the focus down into an nested empty sequence" $ do
    modifyFocus (Timeline () [Sequence () []]) FocusDown (SubFocus 0 SequenceFocus)
      `shouldBe` Left CannotMoveDown

  it "moves the focus down from video into audio in parallel" $ do
    let before' = SubFocus 0 (ClipFocus Video 0)
        after'  = SubFocus 0 (ClipFocus Audio 0)
    modifyFocus singleLevelSequence FocusDown before' `shouldBe` pure after'

  it "moves the focus down from video into audio with nearest start to the left" $ do
    let sequence' = Sequence
          () [ Parallel () [gap3s, video4s] [gap1s, gap3s, audio1s] ]
        before' = SubFocus 0 (ClipFocus Video 1)
        after'  = SubFocus 0 (ClipFocus Audio 1)
    modifyFocus sequence' FocusDown before' `shouldBe` pure after'

  it "moves the focus up from audio into video with same starting point" $ do
    let sequence' = Sequence () [ Parallel () [gap3s, video10s] [gap3s, audio10s] ]
        before' = SubFocus 0 (ClipFocus Video 1)
        after'  = SubFocus 0 (ClipFocus Audio 1)
    modifyFocus sequence' FocusDown before' `shouldBe` pure after'

spec_insertRightOf = do
  it "appends a sequence after the focused one" $ do
    let focus = SubFocus 0 SequenceFocus
        before' = Timeline (1 :: Int) [ Sequence 2 [], Sequence 4 [] ]
        after' = Timeline 1 [ Sequence 2 [], Sequence 3 [], Sequence 4 [] ]
    insert focus (InsertSequence (Sequence 3 [])) RightOf before' `shouldBe` Just after'

  it "appends a video clip after the focused one" $ do
    let focus = SubFocus 0 (SubFocus 0 (ClipFocus Video 0))
        before' = Timeline () [Sequence () [Parallel () [video4s, video10s] []]]
        after' = Timeline () [Sequence () [Parallel () [video4s, gap3s, video10s] []]]
    insert focus (InsertVideoPart gap3s) RightOf before' `shouldBe` Just after'

  it "does not append at the top sequence" $ do
    let focus = SequenceFocus
        before' = Timeline 0 [Sequence (0 :: Int) []]
    insert focus (InsertSequence (Sequence 2 [])) RightOf before' `shouldBe` Nothing

{-# ANN module ("HLint: ignore Use camelCase" :: Prelude.String) #-}
