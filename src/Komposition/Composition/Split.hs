{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TupleSections #-}
-- | Transform a composition by splitting parents.
module Komposition.Composition.Split where

import           Komposition.Prelude

import           Control.Lens
import qualified Data.List.NonEmpty      as NonEmpty

import           Komposition.Composition
import           Komposition.Duration
import           Komposition.Focus
import           Komposition.MediaType

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

splitOnTimeline ::
     focus ~ Focus (ToFocusType Timeline)
  => focus
  -> Timeline a
  -> Maybe (Timeline a, focus)
splitOnTimeline (SequenceFocus sIdx (Just (ParallelFocus pIdx Nothing))) (Timeline seqs) = do
  let newFocus = SequenceFocus sIdx (Just (ParallelFocus (pred pIdx) Nothing))
  Sequence ann pars <- toList seqs `atMay` sIdx
  let (p1, p2) = NonEmpty.splitAt pIdx pars
  ps1 <- nonEmpty p1
  ps2 <- nonEmpty p2
  Just
    ( Timeline (replaceManyAt sIdx [Sequence ann ps1, Sequence ann ps2] seqs)
    , newFocus)
splitOnTimeline (SequenceFocus idx (Just subFocus)) (Timeline seqs) = do
  (seq', newSubFocus) <- splitOnSequence subFocus =<< toList seqs `atMay` idx
  pure (Timeline (seqs & ix idx .~ seq'), SequenceFocus idx (Just newSubFocus))
splitOnTimeline _ _ = mzero

splitOnSequence ::
     focus ~ Focus (ToFocusType Sequence)
  => focus
  -> Sequence a
  -> Maybe (Sequence a, focus)
splitOnSequence (ParallelFocus pIdx (Just (ClipFocus mediaType' cIdx))) (Sequence sAnn pars) = do
  let newFocus = ParallelFocus pIdx (Just (ClipFocus mediaType' (pred cIdx)))
  Parallel pAnn vs as <- toList pars `atMay` pIdx
  splitPars <- case mediaType' of
    Video -> do
      let (vs1, vs2) = splitAt cIdx vs
          (as1, as2) = splitAtDuration (foldMap (durationOf AdjustedDuration) vs1) as
      guard (not (null vs1 || null vs2))
      pure [Parallel pAnn vs1 as1, Parallel pAnn vs2 as2]
    Audio -> do
      let (as1, as2) = splitAt cIdx as
          (vs1, vs2) = splitAtDuration (foldMap (durationOf AdjustedDuration) as1) vs
      guard (not (null as1 || null as2))
      pure [Parallel pAnn vs1 as1, Parallel pAnn vs2 as2]
  pure (Sequence sAnn (replaceManyAt pIdx splitPars pars), newFocus)
splitOnSequence _ _ = mzero

split ::
     Focus (ToFocusType Timeline)
  -> Timeline a
  -> Maybe (Timeline a, Focus (ToFocusType Timeline))
split = splitOnTimeline
