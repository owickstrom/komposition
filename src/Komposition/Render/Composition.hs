{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | Flat compositions for rendering.

module Komposition.Render.Composition
  ( Composition(..)
  , StillFrameMode(..)
  , CompositionPart(..)
  , flattenTimeline
  , flattenSequence
  , flattenParallel
  , singleVideoPart
  ) where

import           Komposition.Prelude

import qualified Komposition.Composition as Core
import           Komposition.Duration
import           Komposition.Library
import           Komposition.MediaType
import           Komposition.VideoSpeed

data Composition =
  Composition (NonEmpty (CompositionPart Video))
              (NonEmpty (CompositionPart Audio))
  deriving (Show, Eq, Generic)

data StillFrameMode = FirstFrame | LastFrame
  deriving (Show, Eq, Generic, Hashable)

data CompositionPart mt where
  VideoClip :: VideoAsset -> TimeSpan -> VideoSpeed -> CompositionPart Video
  StillFrame
    :: StillFrameMode -> VideoAsset -> TimeSpan -> Duration -> CompositionPart Video
  AudioClip :: AudioAsset -> CompositionPart Audio
  Silence :: Duration -> CompositionPart Audio

instance HasDuration (CompositionPart mt) where
  durationOf mode = \case
    VideoClip _ ts speed ->
      case mode of
        AdjustedDuration -> durationInSpeed (durationOf OriginalDuration ts) speed
        OriginalDuration -> durationOf OriginalDuration ts
    AudioClip a -> durationOf OriginalDuration a
    StillFrame _ _ _ d  -> d
    Silence d -> d

deriving instance Eq (CompositionPart t)
deriving instance Show (CompositionPart t)

instance HasDuration Composition where
  durationOf mode (Composition vs as) =
    max (foldMap (durationOf mode) vs) (foldMap (durationOf mode) as)

data Tracks = Tracks [CompositionPart Video] [CompositionPart Audio]
  deriving (Eq, Show)

instance Semigroup Tracks where
  Tracks v1 a1 <> Tracks v2 a2 = Tracks (v1 <> v2) (a1 <> a2)

instance Monoid Tracks where
  mempty = Tracks mempty mempty

instance HasDuration Tracks where
  durationOf mode (Tracks video audio) =
    max (foldMap (durationOf mode) video) (foldMap (durationOf mode) audio)

flattenTimeline :: Core.Timeline a -> Maybe Composition
flattenTimeline (Core.Timeline seqs) = do
  Tracks vs as <- foldMap flattenSequenceTracks seqs
  Composition <$> nonEmpty vs <*> nonEmpty as

flattenSequence :: Core.Sequence a -> Maybe Composition
flattenSequence s = do
  Tracks vs as <- flattenSequenceTracks s
  Composition <$> nonEmpty vs <*> nonEmpty as

flattenParallel :: Core.Parallel a -> Maybe Composition
flattenParallel s = do
  Tracks vs as <- flattenParallelTracks s
  Composition <$> nonEmpty vs <*> nonEmpty as

singleVideoPart :: Core.VideoPart a -> Maybe Composition
singleVideoPart (Core.VideoClip _ asset ts speed) = Just
  (Composition (pure (VideoClip asset ts speed))
               (pure (Silence (durationOf AdjustedDuration ts)))
  )
singleVideoPart _ = Nothing

flattenSequenceTracks :: Core.Sequence a -> Maybe Tracks
flattenSequenceTracks (Core.Sequence _ pars) = foldMap flattenParallelTracks pars

flattenParallelTracks :: Core.Parallel a -> Maybe Tracks
flattenParallelTracks (Core.Parallel _ (Core.VideoTrack _ vs) (Core.AudioTrack _ as)) =
  let (video, lastAsset, lastGaps) =
        foldl' foldVideo (mempty, Nothing, mempty) vs
      audio = foldMap toAudio as
  in matchTrackDurations <$> addGapsWithLastFrame video lastAsset lastGaps <*>
     return audio <*>
     lastAsset
  where
    foldVideo (tracks, lastAsset, precedingGaps) =
      \case
        Core.VideoClip _ asset ts speed ->
          ( tracks <>
            Tracks
              (map (StillFrame FirstFrame asset ts) precedingGaps <>
               [VideoClip asset ts speed])
              []
          , Just (asset, ts)
          , [])
        Core.VideoGap _ d -> (tracks, lastAsset, precedingGaps <> [d])
    toAudio =
      \case
        Core.AudioClip _ asset -> Tracks [] [AudioClip asset]
        Core.AudioGap _ d -> Tracks [] [Silence d]
    matchTrackDurations video audio (lastAsset, ts) =
      case (durationOf AdjustedDuration video, durationOf AdjustedDuration audio) of
        (vd, ad)
          | vd < ad ->
            video <> Tracks [StillFrame LastFrame lastAsset ts (ad - vd)] [] <>
            audio
          | vd > ad -> video <> audio <> Tracks [] [Silence (vd - ad)]
          | otherwise -> video <> audio
    addGapsWithLastFrame videoTrack mLastAsset gaps =
      mLastAsset >>= \(lastAsset, ts) ->
        pure (videoTrack <> Tracks (map (StillFrame LastFrame lastAsset ts) gaps) [])
