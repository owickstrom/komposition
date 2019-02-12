{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Komposition.FocusTest where

import           Komposition.Prelude
import qualified Prelude

import           Control.Lens
import           Test.Tasty.Hspec

import           Komposition.Focus
import           Komposition.MediaType

import           Komposition.TestLibrary

spec_focus_traversal = do
  it "returns focused sequence" $
    timelineTwoParallels
    ^? focusing (SequenceFocus 0 Nothing)
    `shouldBe` Just seqWithTwoParallels
  it "returns focused parallel" $
    timelineTwoParallels
    ^? focusing (SequenceFocus 0 (Just (ParallelFocus 1 Nothing)))
    `shouldBe` Just parallel2
  it "returns focused video clip" $
    timelineTwoParallels
    ^? focusing (SequenceFocus 0 (Just (ParallelFocus 1 (Just (TrackFocus Video (Just (ClipFocus 0)))))))
    `shouldBe` Just videoGap3s
  it "returns focused audio clip" $
    timelineTwoParallels
    ^? focusing (SequenceFocus 0 (Just (ParallelFocus 1 (Just (TrackFocus Audio (Just (ClipFocus 1)))))))
    `shouldBe` Just audio10s

spec_modify_focus = do
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
  it "moves the focus left within parallel" $ do
    let before' = SequenceFocus 0 (Just (ParallelFocus 1 Nothing))
        after'  = SequenceFocus 0 (Just (ParallelFocus 0 Nothing))
    modifyFocus timelineTwoParallels FocusLeft before' `shouldBe` pure after'
  it "moves out to track focus when moving left from first video clip" $ do
    let before' = ParallelFocus 0 (Just (TrackFocus Video (Just (ClipFocus 0))))
        after' = ParallelFocus 0 (Just (TrackFocus Video Nothing))
    modifyFocus seqWithTwoParallels FocusLeft before' `shouldBe` Right after'
  it "moves out to track focus when moving left from first audio clip" $ do
    let before' = ParallelFocus 0 (Just (TrackFocus Audio (Just (ClipFocus 0))))
        after' = ParallelFocus 0 (Just (TrackFocus Audio Nothing))
    modifyFocus seqWithTwoParallels FocusLeft before' `shouldBe` Right after'
  it "maintains parallel focus when right move is out of bounds" $ do
    let before' = ParallelFocus 0 (Just (TrackFocus Video (Just (ClipFocus 1))))
    modifyFocus seqWithTwoParallels FocusRight before'
      `shouldBe` Left OutOfBounds
  it "moves the focus up from parallel up to sequence" $ do
    let before' = SequenceFocus 0 (Just (ParallelFocus 1 Nothing))
        after'  = SequenceFocus 0 Nothing
    modifyFocus timelineTwoParallels FocusUp before' `shouldBe` pure after'
  it "maintains top sequence focus when trying to move up" $ do
    let before' = SequenceFocus 0 Nothing
    modifyFocus timelineTwoParallels FocusUp before'
      `shouldBe` Left (CannotMove FocusUp)
  it "moves the focus down from sequence to a parallel" $ do
    let before' = SequenceFocus 0 Nothing
        after'  = SequenceFocus 0 (Just (ParallelFocus 0 Nothing))
    modifyFocus timelineTwoParallels FocusDown before' `shouldBe` pure after'
  it "moves the focus down from parallel to video track" $ do
    let before' = SequenceFocus 0 (Just (ParallelFocus 0 Nothing))
        after' =
          SequenceFocus 0 (Just (ParallelFocus 0 (Just (TrackFocus Video Nothing))))
    modifyFocus timelineTwoParallels FocusDown before' `shouldBe` pure after'
  it "moves the focus down from video track to audio track" $ do
    let before' = SequenceFocus 0 (Just (ParallelFocus 0 (Just (TrackFocus Video Nothing))))
        after' =
          SequenceFocus 0 (Just (ParallelFocus 0 (Just (TrackFocus Audio Nothing))))
    modifyFocus timelineTwoParallels FocusDown before' `shouldBe` pure after'
  it "cannot the focus down from video clip" $ do
    let before' = ParallelFocus 0 (Just (TrackFocus Video (Just (ClipFocus 0))))
    modifyFocus seqWithTwoParallels FocusDown before'
      `shouldBe` Left (CannotMove FocusDown)
  it "cannot the focus down from audio clip" $ do
    let before' = ParallelFocus 0 (Just (TrackFocus Audio (Just (ClipFocus 0))))
    modifyFocus seqWithTwoParallels FocusDown before'
      `shouldBe` Left (CannotMove FocusDown)

{-# ANN module ("HLint: ignore Use camelCase" :: Prelude.String) #-}
