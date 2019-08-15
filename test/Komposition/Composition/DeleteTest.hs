{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Komposition.Composition.DeleteTest where

import           Komposition.Prelude
import qualified Prelude

import           Data.Tree
import           Hedgehog                           hiding (Parallel)
import           Hedgehog.Range
import           Test.Tasty.Hspec

import           Komposition.Composition
import           Komposition.Composition.Delete     as Delete
import qualified Komposition.Composition.Generators as Gen
import           Komposition.Composition.Insert     as Insert
import           Komposition.Composition.ToTree
import           Komposition.Focus
import           Komposition.MediaType

import           Komposition.TestLibrary

spec_delete = do
  it "deletes only audio part and retains valid focus" $ do
    let
      focusBefore = SequenceFocus
        0
        (Just (ParallelFocus 0 (Just (TrackFocus Audio (Just (ClipFocus 0))))))
      before' = Timeline
        (pure
          (Sequence () (pure (Parallel () mempty (AudioTrack () [audio1s]))))
        )
      focusAfter = SequenceFocus
        0
        (Just (ParallelFocus 0 (Just (TrackFocus Audio Nothing))))
      after' = Timeline (pure (Sequence () (pure (Parallel () mempty mempty))))
    delete focusBefore (DeletionOf 1) before' `shouldBe` Just
      (DeletionResult after'
                      (InsertAudioParts (pure audio1s), RightOf)
                      focusAfter
      )
  it "deletes last focused parallel" $ do
    let
      focusBefore = SequenceFocus
        0
        (Just (ParallelFocus 1 Nothing))
      before' = Timeline
        (pure
          (Sequence () (parallel1 :| [parallel2]))
        )
      focusAfter = SequenceFocus
        0
        (Just (ParallelFocus 0 Nothing))
      after' = Timeline (pure (Sequence () (pure parallel1)))
    delete focusBefore (DeletionOf 1) before' `shouldBe` Just
      (DeletionResult after'
                      (InsertParallel parallel2, RightOf)
                      focusAfter
      )

hprop_returned_inverse_insertion_is_an_undo_action = property $ do
  (initialTimeline, initialFocus) <-
    forAllWith showTimelineAndFocus (Gen.timelineWithFocus (linear 0 3) Gen.parallel)
  case delete initialFocus (DeletionOf 1) initialTimeline of
    Nothing -> pass
    Just (DeletionResult newTimeline (insertion, insertPos) newFocus) ->
      case insert newFocus insertion insertPos newTimeline of
        Just (InsertionResult finalTimeline _finalFocus) ->
          timelineToTree initialTimeline === timelineToTree finalTimeline
        Nothing -> failure
  where
    showTimelineAndFocus (t, f) = drawTree (timelineToTree t) <> "\n" <> show f

{-# ANN module ("HLint: ignore Use camelCase" :: Prelude.String) #-}
