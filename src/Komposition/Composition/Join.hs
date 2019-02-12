module Komposition.Composition.Join (JoinResult (..), join) where

import           Komposition.Prelude           hiding (join)

import           Control.Lens
import qualified Data.List.NonEmpty            as NonEmpty

import           Komposition.Composition
import           Komposition.Composition.Split
import           Komposition.Focus
import           Komposition.MediaType

data JoinResult comp = JoinResult
  { resultingComposition :: comp ()
  , resultingFocus       :: Focus (ToFocusType comp)
  , inverseSplit         :: (Focus (ToFocusType comp), ParallelSplitMode)
  }

join
  :: SequenceFocus
  -> Timeline ()
  -> Maybe (JoinResult Timeline)
join oldFocus@(SequenceFocus sIdx Nothing) (Timeline seqs) = do
  Sequence _ pars <- toList seqs `atMay` sIdx
  let splitFocus = SequenceFocus sIdx (Just (ParallelFocus (length pars) Nothing))
  newTimeline <- Timeline <$> joinAt sIdx seqs
  pure (JoinResult newTimeline oldFocus (splitFocus, OnClipsNearFocus))
join (SequenceFocus sIdx (Just subFocus)) (Timeline seqs) = do
  result <- joinParallels subFocus =<< toList seqs `atMay` sIdx

  pure
    (JoinResult (Timeline (seqs & ix sIdx .~ resultingComposition result))
                (SequenceFocus sIdx (Just (resultingFocus result)))
                (inverseSplit result & _1 %~ SequenceFocus sIdx . Just)
    )

joinParallels
  :: ParallelFocus
  -> Sequence ()
  -> Maybe (JoinResult Sequence)
joinParallels oldFocus@(ParallelFocus pIdx Nothing) (Sequence ann pars) = do
  Parallel _ (VideoTrack _ vs) (AudioTrack _ as) <- toList pars `atMay` pIdx
  let splitFocus = ParallelFocus pIdx (Just (TrackFocus Video (Just (ClipFocus (length vs)))))
  newSequence <- Sequence ann <$> joinAt pIdx pars
  pure (JoinResult newSequence oldFocus (splitFocus, OnExactClips (length vs) (length as)))
joinParallels (ParallelFocus _ (Just _)) _ = mzero

joinAt :: Semigroup a => Int -> NonEmpty a -> Maybe (NonEmpty a)
joinAt i xs =
  case NonEmpty.splitAt i xs of
    (before, left:right:rest) -> nonEmpty (before <> pure (left <> right) <> rest)
    _                         -> mzero
