{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
-- | Annotate a composition with focus metadata.

module Komposition.Composition.Focused where

import           Komposition.Prelude

import           Data.List.NonEmpty      (NonEmpty (..))
import qualified Data.List.NonEmpty      as NonEmpty

import           Komposition.Composition
import           Komposition.Focus
import           Komposition.MediaType

data Focused
  = Focused
  | TransitivelyFocused
  | Blurred
  deriving (Eq, Show)

-- | Given a current focus, returns whether the other focus is focused
-- (the same as the first), transitively focused (a sub-path of the
-- current focus), or not focused at all.
focusedState
  :: SequenceFocus -- ^ Current focus
  -> SequenceFocus -- ^ A focus to check
  -> Focused
focusedState f1 f2 = onSequenceFocus (f1, f2)
  where
    onSequenceFocus (SequenceFocus i1 mf1, SequenceFocus i2 mf2)
      | i1 == i2 = subFocusState onParallelFocus (mf1, mf2)
      | otherwise = Blurred
    onParallelFocus (ParallelFocus i1 mf1, ParallelFocus i2 mf2)
      | i1 == i2 = subFocusState onTrackFocus (mf1, mf2)
      | otherwise = Blurred
    onTrackFocus (TrackFocus mt1 cf1, TrackFocus mt2 cf2)
      | mt1 == mt2 = subFocusState onClipFocus (cf1, cf2)
      | otherwise = Blurred
    onClipFocus (ClipFocus i1, ClipFocus i2)
      | i1 == i2 = Focused
      | otherwise = Blurred
    subFocusState f =
      \case
        (Nothing, Nothing) -> Focused
        (Just _, Nothing) -> TransitivelyFocused
        (Just f1', Just f2') -> f (f1', f2')
        (Nothing, Just _) -> Blurred

numsFromZero :: (Enum n, Num n) => NonEmpty n
numsFromZero = 0 :| [1 ..]

withAllFoci :: Timeline a -> Timeline (Focus 'SequenceFocusType)
withAllFoci (Timeline sub) =
  Timeline
    (NonEmpty.zipWith (onSequence . SequenceFocus) numsFromZero sub)
  where
    onSequence ::
         (Maybe (Focus 'ParallelFocusType) -> Focus 'SequenceFocusType)
      -> Sequence a
      -> Sequence (Focus 'SequenceFocusType)
    onSequence wrap (Sequence _ pars) =
      Sequence
        (wrap Nothing)
        (NonEmpty.zipWith
           (\i -> onParallel (wrap . Just . ParallelFocus i))
           numsFromZero
           pars)
    onParallel ::
         (Maybe (Focus 'TrackFocusType) -> Focus 'SequenceFocusType)
      -> Parallel a
      -> Parallel (Focus 'SequenceFocusType)
    onParallel wrap (Parallel _ videoTrack' audioTrack') =
        Parallel
        (wrap Nothing)
        (onVideoTrack (wrap . Just . TrackFocus Video) videoTrack')
        (onAudioTrack (wrap . Just . TrackFocus Audio) audioTrack')
    onVideoTrack wrap (VideoTrack _ parts') =
      VideoTrack (wrap Nothing) (zipWith (onTrackPart . wrap . Just . ClipFocus) [0 ..] parts')
    onAudioTrack wrap (AudioTrack _ parts') =
      AudioTrack (wrap Nothing) (zipWith (onTrackPart . wrap . Just . ClipFocus) [0 ..] parts')
    onTrackPart focus = ($> focus)
