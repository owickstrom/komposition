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

seqWithTwoParallels = Sequence
  ()
  [ Parallel () [gap1s, video4s] [audio1s]
  , Parallel () [gap3s, video10s] [audio4s, audio10s]
  ]

timelineTwoParallels =
  Timeline () [Sequence () [], Sequence () [parallel1, parallel2]]

spec_applyFocus = do

  it "applies focus to sequence"
    $          applyFocus (Timeline () [Sequence () [Parallel () [Gap () 1] []]])
                          (SequenceFocus 0 Nothing)
    `shouldBe` Timeline TransitivelyFocused [Sequence Focused [Parallel Blurred [Gap Blurred 1] []]]

  it "applies focus to parallel"
    $          applyFocus (Timeline () [Sequence () [Parallel () [Gap () 1] []]])
                          (SequenceFocus 0 (Just (ParallelFocus 0 Nothing)))
    `shouldBe` Timeline TransitivelyFocused [Sequence TransitivelyFocused [Parallel Focused [Gap Blurred 1] []]]

spec_modifyFocus = do

  it "moves the focus left within a sequence" $ do
    let before' = ParallelFocus 1 Nothing
        after'  = ParallelFocus 0 Nothing
    modifyFocus seqWithTwoParallels FocusLeft before' `shouldBe` pure after'

  it "maintains sequence focus when left move is out of bounds" $ do
    let before' = ParallelFocus 0 Nothing
    modifyFocus seqWithTwoParallels FocusLeft before'
      `shouldBe` Left OutOfBounds

  it "maintains sequence focus when right move is out of bounds" $ do
    let before' = ParallelFocus 1 Nothing
    modifyFocus seqWithTwoParallels FocusRight before'
      `shouldBe` Left OutOfBounds

  it "moves the focus left within parallel"
    $ do
        let before' = SequenceFocus 1 (Just (ParallelFocus 1 Nothing))
            after'  = SequenceFocus 1 (Just (ParallelFocus 0 Nothing))
        modifyFocus timelineTwoParallels FocusLeft before' `shouldBe` pure after'

  it "maintains parallel focus when left move is out of bounds" $ do
    let before' = ParallelFocus 0 (Just (ClipFocus Video 0))
    modifyFocus seqWithTwoParallels FocusLeft before' `shouldBe` Left
      OutOfBounds

  it "maintains parallel focus when right move is out of bounds" $ do
    let before' = ParallelFocus 0 (Just (ClipFocus Video 1))
    modifyFocus seqWithTwoParallels FocusRight before' `shouldBe` Left
      OutOfBounds

  it "moves the focus up from parallel up to sequence" $ do
    let before' = SequenceFocus 1 (Just (ParallelFocus 1 Nothing))
        after'  = SequenceFocus 1 Nothing
    modifyFocus timelineTwoParallels FocusUp before' `shouldBe` pure after'

  it "moves the focus up from video in multi-level sequence" $ do
    let before' = SequenceFocus
          1
          (Just (ParallelFocus 1 (Just (ClipFocus Video 1))))
        after' = SequenceFocus 1 (Just (ParallelFocus 1 Nothing))
    modifyFocus timelineTwoParallels FocusUp before' `shouldBe` pure after'

  it "moves the focus up from audio into video with nearest start to the left" $ do
    let sequence' = Sequence
          () [ Parallel () [gap3s, video4s] [gap1s, gap3s, audio1s] ]
        before' = ParallelFocus 0 (Just (ClipFocus Audio 2))
        after'  = ParallelFocus 0 (Just (ClipFocus Video 1))
    modifyFocus sequence' FocusUp before' `shouldBe` pure after'

  it "moves the focus up from audio into video with same starting point" $ do
    let sequence' = Sequence () [ Parallel () [gap3s, video10s] [gap3s, audio10s] ]
        before' = ParallelFocus 0 (Just (ClipFocus Audio 1))
        after'  = ParallelFocus 0 (Just (ClipFocus Video 1))
    modifyFocus sequence' FocusUp before' `shouldBe` pure after'

  it "maintains top sequence focus when trying to move up" $ do
    let before' = SequenceFocus 0 Nothing
    modifyFocus timelineTwoParallels FocusUp before' `shouldBe` Left CannotMoveUp

  it "moves the focus down from sequence to a parallel" $ do
    let before' = SequenceFocus 1 Nothing
        after'  = SequenceFocus 1 (Just (ParallelFocus 0 Nothing))
    modifyFocus timelineTwoParallels FocusDown before' `shouldBe` pure after'

  it "moves the focus down from parallel to first video clip" $ do
    let before' = SequenceFocus 1 (Just (ParallelFocus 0 Nothing))
        after'  = SequenceFocus 1 (Just (ParallelFocus 0 (Just (ClipFocus Video 0))))
    modifyFocus timelineTwoParallels FocusDown before' `shouldBe` pure after'

  it "cannot move the focus down into an empty sequence" $
    modifyFocus (Timeline () [Sequence () []]) FocusDown (SequenceFocus 0 Nothing)
      `shouldBe` Left CannotMoveDown

  it "cannot move the focus down into an empty sequence" $
    modifyFocus (Timeline () [Sequence () []]) FocusDown (SequenceFocus 0 Nothing)
      `shouldBe` Left CannotMoveDown

  it "moves the focus down from video into audio in parallel" $ do
    let before' = ParallelFocus 0 (Just (ClipFocus Video 0))
        after'  = ParallelFocus 0 (Just (ClipFocus Audio 0))
    modifyFocus seqWithTwoParallels FocusDown before' `shouldBe` pure after'

  it "cannot the focus down from audio" $ do
    let before' = ParallelFocus 0 (Just (ClipFocus Audio 0))
    modifyFocus seqWithTwoParallels FocusDown before'
      `shouldBe` Left CannotMoveDown

  it "moves the focus down from video into audio with nearest start to the left" $ do
    let sequence' = Sequence
          () [ Parallel () [gap3s, video4s] [gap1s, gap3s, audio1s] ]
        before' = ParallelFocus 0 (Just (ClipFocus Video 1))
        after'  = ParallelFocus 0 (Just (ClipFocus Audio 1))
    modifyFocus sequence' FocusDown before' `shouldBe` pure after'

  it "moves the focus up from audio into video with same starting point" $ do
    let sequence' = Sequence () [ Parallel () [gap3s, video10s] [gap3s, audio10s] ]
        before' = ParallelFocus 0 (Just (ClipFocus Video 1))
        after'  = ParallelFocus 0 (Just (ClipFocus Audio 1))
    modifyFocus sequence' FocusDown before' `shouldBe` pure after'

spec_insertRightOf = do
  it "appends a sequence after the focused one" $ do
    let focus = SequenceFocus 0 Nothing
        before' = Timeline (1 :: Int) [ Sequence 2 [], Sequence 4 [] ]
        after' = Timeline 1 [ Sequence 2 [], Sequence 3 [], Sequence 4 [] ]
    insert focus (InsertSequence (Sequence 3 [])) RightOf before' `shouldBe` Just after'

  it "appends a video clip after the focused one" $ do
    let focus = SequenceFocus 0 (Just (ParallelFocus 0 (Just (ClipFocus Video 0))))
        before' = Timeline () [Sequence () [Parallel () [video4s, video10s] []]]
        after' = Timeline () [Sequence () [Parallel () [video4s, gap3s, video10s] []]]
    insert focus (InsertVideoPart gap3s) RightOf before' `shouldBe` Just after'

{-# ANN module ("HLint: ignore Use camelCase" :: Prelude.String) #-}
