{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module FastCut.FocusTest where

import           FastCut.Prelude
import qualified Prelude

import           Test.Tasty.Hspec

import           FastCut.Composition
import           FastCut.Focus
import           FastCut.MediaType

import           FastCut.TestLibrary

spec_atFocus = do
  it "returns focused sequence" $
    atFocus (SequenceFocus 0 Nothing) timelineTwoParallels `shouldBe`
    Just (FocusedSequence (Sequence () []))
  it "returns focused parallel" $
    atFocus
      (SequenceFocus 1 (Just (ParallelFocus 1 Nothing)))
      timelineTwoParallels `shouldBe`
    Just (FocusedParallel parallel2)
  it "returns focused video clip" $
    atFocus
      (SequenceFocus 1 (Just (ParallelFocus 1 (Just (ClipFocus Video 0)))))
      timelineTwoParallels `shouldBe`
    Just (FocusedVideoPart gap3s)
  it "returns focused audio clip" $
    atFocus
      (SequenceFocus 1 (Just (ParallelFocus 1 (Just (ClipFocus Audio 1)))))
      timelineTwoParallels `shouldBe`
    Just (FocusedAudioPart audio10s)

spec_modifyFocus = do
  it "moves the focus left within a sequence" $ do
    let before' = ParallelFocus 1 Nothing
        after' = ParallelFocus 0 Nothing
    modifyFocus seqWithTwoParallels FocusLeft before' `shouldBe` pure after'
  it "maintains sequence focus when left move is out of bounds" $ do
    let before' = ParallelFocus 0 Nothing
    modifyFocus seqWithTwoParallels FocusLeft before' `shouldBe`
      Left OutOfBounds
  it "maintains sequence focus when right move is out of bounds" $ do
    let before' = ParallelFocus 1 Nothing
    modifyFocus seqWithTwoParallels FocusRight before' `shouldBe`
      Left OutOfBounds
  it "moves the focus left within parallel" $ do
    let before' = SequenceFocus 1 (Just (ParallelFocus 1 Nothing))
        after' = SequenceFocus 1 (Just (ParallelFocus 0 Nothing))
    modifyFocus timelineTwoParallels FocusLeft before' `shouldBe` pure after'
  it "maintains parallel focus when left move is out of bounds" $ do
    let before' = ParallelFocus 0 (Just (ClipFocus Video 0))
    modifyFocus seqWithTwoParallels FocusLeft before' `shouldBe`
      Left OutOfBounds
  it "maintains parallel focus when right move is out of bounds" $ do
    let before' = ParallelFocus 0 (Just (ClipFocus Video 1))
    modifyFocus seqWithTwoParallels FocusRight before' `shouldBe`
      Left OutOfBounds
  it "moves the focus up from parallel up to sequence" $ do
    let before' = SequenceFocus 1 (Just (ParallelFocus 1 Nothing))
        after' = SequenceFocus 1 Nothing
    modifyFocus timelineTwoParallels FocusUp before' `shouldBe` pure after'
  it "moves the focus up from video in multi-level sequence" $ do
    let before' =
          SequenceFocus 1 (Just (ParallelFocus 1 (Just (ClipFocus Video 1))))
        after' = SequenceFocus 1 (Just (ParallelFocus 1 Nothing))
    modifyFocus timelineTwoParallels FocusUp before' `shouldBe` pure after'
  it "moves the focus up from audio into video with nearest start to the left" $ do
    let sequence' =
          Sequence () [Parallel () [gap3s, video4s] [gap1s, gap3s, audio1s]]
        before' = ParallelFocus 0 (Just (ClipFocus Audio 2))
        after' = ParallelFocus 0 (Just (ClipFocus Video 1))
    modifyFocus sequence' FocusUp before' `shouldBe` pure after'
  it "moves the focus up from audio into video with same starting point" $ do
    let sequence' =
          Sequence () [Parallel () [gap3s, video10s] [gap3s, audio10s]]
        before' = ParallelFocus 0 (Just (ClipFocus Audio 1))
        after' = ParallelFocus 0 (Just (ClipFocus Video 1))
    modifyFocus sequence' FocusUp before' `shouldBe` pure after'
  it "maintains top sequence focus when trying to move up" $ do
    let before' = SequenceFocus 0 Nothing
    modifyFocus timelineTwoParallels FocusUp before' `shouldBe`
      Left CannotMoveUp
  it "moves the focus down from sequence to a parallel" $ do
    let before' = SequenceFocus 1 Nothing
        after' = SequenceFocus 1 (Just (ParallelFocus 0 Nothing))
    modifyFocus timelineTwoParallels FocusDown before' `shouldBe` pure after'
  it "moves the focus down from parallel to first video clip" $ do
    let before' = SequenceFocus 1 (Just (ParallelFocus 0 Nothing))
        after' =
          SequenceFocus 1 (Just (ParallelFocus 0 (Just (ClipFocus Video 0))))
    modifyFocus timelineTwoParallels FocusDown before' `shouldBe` pure after'
  it "cannot move the focus down into an empty sequence" $
    modifyFocus
      (Timeline () [Sequence () []])
      FocusDown
      (SequenceFocus 0 Nothing) `shouldBe`
    Left CannotMoveDown
  it "cannot move the focus down into an empty sequence" $
    modifyFocus
      (Timeline () [Sequence () []])
      FocusDown
      (SequenceFocus 0 Nothing) `shouldBe`
    Left CannotMoveDown
  it "moves the focus down from video into audio in parallel" $ do
    let before' = ParallelFocus 0 (Just (ClipFocus Video 0))
        after' = ParallelFocus 0 (Just (ClipFocus Audio 0))
    modifyFocus seqWithTwoParallels FocusDown before' `shouldBe` pure after'
  it "cannot the focus down from audio" $ do
    let before' = ParallelFocus 0 (Just (ClipFocus Audio 0))
    modifyFocus seqWithTwoParallels FocusDown before' `shouldBe`
      Left CannotMoveDown
  it "moves the focus down from video into audio with nearest start to the left" $ do
    let sequence' =
          Sequence () [Parallel () [gap3s, video4s] [gap1s, gap3s, audio1s]]
        before' = ParallelFocus 0 (Just (ClipFocus Video 1))
        after' = ParallelFocus 0 (Just (ClipFocus Audio 1))
    modifyFocus sequence' FocusDown before' `shouldBe` pure after'
  it "moves the focus up from audio into video with same starting point" $ do
    let sequence' =
          Sequence () [Parallel () [gap3s, video10s] [gap3s, audio10s]]
        before' = ParallelFocus 0 (Just (ClipFocus Video 1))
        after' = ParallelFocus 0 (Just (ClipFocus Audio 1))
    modifyFocus sequence' FocusDown before' `shouldBe` pure after'

{-# ANN module ("HLint: ignore Use camelCase" :: Prelude.String) #-}
