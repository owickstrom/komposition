{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs         #-}
-- | Transform a composition by splitting parents.
module Komposition.Composition.Split where

import           Komposition.Prelude

import           Control.Lens
import qualified Data.List.NonEmpty      as NonEmpty

import           Komposition.Composition
import           Komposition.Duration
import           Komposition.Focus
import           Komposition.MediaType

data ParallelSplitMode = OnExactClips Int Int | OnClipsNearFocus
  deriving (Eq, Show, Generic)

split ::
     ParallelSplitMode
  -> Focus (ToFocusType Timeline)
  -> Timeline a
  -> Maybe (Timeline a, Focus (ToFocusType Timeline))
split = splitOnTimeline

splitOnTimeline
  :: ParallelSplitMode
  -> SequenceFocus
  -> Timeline a
  -> Maybe (Timeline a, SequenceFocus)
splitOnTimeline _ (SequenceFocus sIdx (Just (ParallelFocus pIdx Nothing))) (Timeline seqs) = do
  let newFocus = SequenceFocus sIdx (Just (ParallelFocus (pred pIdx) Nothing))
  Sequence ann pars <- toList seqs `atMay` sIdx
  let (p1, p2) = NonEmpty.splitAt pIdx pars
  ps1 <- nonEmpty p1
  ps2 <- nonEmpty p2
  Just
    ( Timeline (replaceManyAt sIdx [Sequence ann ps1, Sequence ann ps2] seqs)
    , newFocus)
splitOnTimeline splitMode (SequenceFocus idx (Just subFocus)) (Timeline seqs) = do
  (seq', newSubFocus) <- splitOnSequence splitMode subFocus =<< toList seqs `atMay` idx
  pure (Timeline (seqs & ix idx .~ seq'), SequenceFocus idx (Just newSubFocus))
splitOnTimeline _ _ _ = mzero

splitOnSequence
  :: ParallelSplitMode
  -> ParallelFocus
  -> Sequence a
  -> Maybe (Sequence a, ParallelFocus)
splitOnSequence splitMode (ParallelFocus pIdx (Just (TrackFocus mediaType' (Just (ClipFocus cIdx))))) (Sequence sAnn pars) = do
    let newFocus = ParallelFocus
          pIdx
          (Just (TrackFocus mediaType' (Just (ClipFocus (pred cIdx)))))
    Parallel pAnn (VideoTrack vAnn vs) (AudioTrack aAnn as) <-
      toList pars `atMay` pIdx
    let ((vs1, vs2), (as1, as2)) = case (splitMode, mediaType') of
          (OnClipsNearFocus, Video) -> splitBasedOnFirst cIdx vs as
          (OnClipsNearFocus, Audio) -> swap (splitBasedOnFirst cIdx as vs)
          (OnExactClips v a, _    ) -> (splitAt v vs, splitAt a as)
    guard (not (null vs1 || null vs2))
    let splitPars =
          [ Parallel pAnn (VideoTrack vAnn vs1) (AudioTrack aAnn as1)
          , Parallel pAnn (VideoTrack vAnn vs2) (AudioTrack aAnn as2)
          ]
    pure (Sequence sAnn (replaceManyAt pIdx splitPars pars), newFocus)
splitOnSequence _ _ _ = mzero

splitBasedOnFirst
  :: (HasDuration t1, HasDuration t2)
  => Int
  -> [t1]
  -> [t2]
  -> (([t1], [t1]), ([t2], [t2]))
splitBasedOnFirst idx t1 t2 =
  let split1 = splitAt idx t1
      split2 = splitAtDuration (foldMap (durationOf AdjustedDuration) (fst split1)) t2
  in (split1, split2)

replaceManyAt :: Int -> [a] -> NonEmpty a -> NonEmpty a
replaceManyAt i new xs =
  let (before, after) = NonEmpty.splitAt i xs
  in  NonEmpty.fromList (before <> new <> drop 1 after)

splitAtDuration :: HasDuration clip => Duration -> [clip] -> ([clip], [clip])
splitAtDuration d = go 0 ([], [])
  where
    go _ acc [] = acc
    go current (before, after) cs@(clip:clips)
      | current >= d = (before, after <> cs)
      | otherwise =
        go (current <> durationOf AdjustedDuration clip) (before <> [clip], after) clips
