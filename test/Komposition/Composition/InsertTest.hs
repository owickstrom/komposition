{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Komposition.Composition.InsertTest where

import           Komposition.Prelude
import qualified Prelude

import qualified Data.List.NonEmpty                 as NonEmpty
import           Data.Tree
import           Hedgehog                           hiding (Parallel)
import qualified Hedgehog.Gen                       as Gen hiding (parallel)
import           Hedgehog.Range
import           Test.Tasty.Hspec

import           Komposition.Composition
import qualified Komposition.Composition.Generators as Gen
import           Komposition.Composition.Insert
import           Komposition.Composition.ToTree
import           Komposition.Focus
import           Komposition.MediaType

import           Komposition.TestLibrary

spec_insertRightOf = do
  it "appends a sequence after the focused one" $ do
    let focus' = SequenceFocus 1 Nothing
        before' =
          Timeline
            (Sequence () (pure parallel1) :| [Sequence () (pure parallel2)])
        after' =
          Timeline
            (Sequence () (pure parallel1) :|
             [Sequence () (pure parallel2), Sequence () (pure parallel1)])
    insert_ focus' (InsertSequence (Sequence () (pure parallel1))) RightOf before' `shouldBe`
      Just (after', SequenceFocus 2 Nothing)

  it "appends a parallel after the focused last one" $ do
    let focus' = SequenceFocus 0 (Just (ParallelFocus 1 Nothing))
        before' =
          Timeline
            (pure (Sequence () (parallel1 :| [parallel2])))
        after' =
          Timeline
            (pure (Sequence () (parallel1 :| [parallel2, parallel1])))
    insert_ focus' (InsertParallel parallel1) RightOf before' `shouldBe`
      Just (after', SequenceFocus 0 (Just (ParallelFocus 2 Nothing)))

  it "appends a video clip after the focused one" $ do
    let focus' =
          SequenceFocus 0 (Just (ParallelFocus 0 (Just (TrackFocus Video (Just (ClipFocus 0))))))
        before' =
          Timeline
            (pure (Sequence () (pure (Parallel () (VideoTrack () [video4s, video10s]) mempty))))
        after' =
          Timeline
            (pure
               (Sequence () (pure (Parallel () (VideoTrack () [video4s, videoGap3s, video10s]) mempty))))
    insert_ focus' (InsertVideoParts (pure videoGap3s)) RightOf before' `shouldBe`
      Just (after', SequenceFocus 0 (Just (ParallelFocus 0 (Just (TrackFocus Video (Just (ClipFocus 1)))))))

  it "insert a video clip into an empty parallel" $ do
    let focus' =
          SequenceFocus 0 (Just (ParallelFocus 0 Nothing))
        before' =
          Timeline
            (pure (Sequence () (pure (Parallel () mempty mempty))))
        after' =
          Timeline
            (pure
               (Sequence () (pure (Parallel () (VideoTrack () [video4s]) mempty))))
    insert_ focus' (InsertVideoParts (pure video4s)) LeftMost before' `shouldBe`
      Just (after', SequenceFocus 0 (Just (ParallelFocus 0 (Just (TrackFocus Video (Just (ClipFocus 0)))))))

  it "insert an audio clip into an empty parallel" $ do
    let focus' =
          SequenceFocus 0 (Just (ParallelFocus 0 Nothing))
        before' =
          Timeline
            (pure (Sequence () (pure (Parallel () mempty mempty))))
        after' =
          Timeline
            (pure
               (Sequence () (pure (Parallel () (VideoTrack () [video4s]) mempty))))
    insert_ focus' (InsertVideoParts (pure video4s)) LeftMost before' `shouldBe`
      Just (after', SequenceFocus 0 (Just (ParallelFocus 0 (Just (TrackFocus Video (Just (ClipFocus 0)))))))

  it "insert a video clip into an empty video track" $ do
    let focus' =
          SequenceFocus 0 (Just (ParallelFocus 0 (Just (TrackFocus Video Nothing))))
        before' =
          Timeline
            (pure (Sequence () (pure (Parallel () mempty mempty))))
        after' =
          Timeline
            (pure
               (Sequence () (pure (Parallel () (VideoTrack () [video4s]) mempty))))
    insert_ focus' (InsertVideoParts (pure video4s)) LeftMost before' `shouldBe`
      Just (after', SequenceFocus 0 (Just (ParallelFocus 0 (Just (TrackFocus Video (Just (ClipFocus 0)))))))

  it "insert an audio clip into an empty audio track" $ do
    let focus' =
          SequenceFocus 0 (Just (ParallelFocus 0 (Just (TrackFocus Audio Nothing))))
        before' =
          Timeline
            (pure (Sequence () (pure (Parallel () mempty mempty))))
        after' =
          Timeline
            (pure
               (Sequence () (pure (Parallel () mempty (AudioTrack () [audio1s])))))
    insert_ focus' (InsertAudioParts (pure audio1s)) LeftMost before' `shouldBe`
      Just (after', SequenceFocus 0 (Just (ParallelFocus 0 (Just (TrackFocus Audio (Just (ClipFocus 0)))))))


hprop_insert_always_changes_focus_to_the_newly_inserted = property $ do
  (tl, oldFocus) <- forAllWith showTimelineAndFocus (Gen.timelineWithFocus (linear 0 3) Gen.parallel)
  (insertion, insertPos) <- forAll (Gen.insertion oldFocus)
  case insert_ oldFocus insertion insertPos tl of
    Nothing                      -> failure
    Just (newTimeline, newFocus) -> do
      annotateShow newFocus
      case atFocus newFocus newTimeline of
        Just someComposition -> someCompositionToTree (firstInInsert insertion)
          === someCompositionToTree someComposition
        Nothing -> failure
  where
    firstInInsert = \case
      InsertSequence   s        -> SomeSequence s
      InsertParallel   s        -> SomeParallel s
      InsertVideoParts (v :| _) -> SomeVideoPart v
      InsertAudioParts (a :| _) -> SomeAudioPart a
    showTimelineAndFocus (t, f) = drawTree (timelineToTree t) <> "\n" <> show f


{-# ANN module ("HLint: ignore Use camelCase" :: Prelude.String) #-}
