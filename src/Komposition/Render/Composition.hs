{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
-- | Flat compositions for rendering.

module Komposition.Render.Composition
  ( VideoClip (..)
  , StillFrame (..)
  , stillFrameMode
  , stillFrameAsset
  , stillFrameTimeSpan
  , stillFrameDuration
  , Composition(..)
  , videoParts
  , audioParts
  , StillFrameMode(..)
  , CompositionPart(..)
  , _VideoClipPart
  , _StillFramePart
  , _AudioClipPart
  , _SilencePart
  , flattenTimeline
  , flattenSequence
  , flattenParallel
  , singleVideoPart
  ) where

import           Komposition.Prelude

import           Control.Lens

import qualified Komposition.Composition as Core
import           Komposition.Duration
import           Komposition.Library
import           Komposition.MediaType
import           Komposition.VideoSpeed

data StillFrameMode = FirstFrame | LastFrame
  deriving (Show, Eq, Generic, Hashable)

data VideoClip = VideoClip VideoAsset TimeSpan VideoSpeed
  deriving (Show, Eq, Generic)

data StillFrame = StillFrame { _stillFrameMode     :: StillFrameMode
                             , _stillFrameAsset    :: VideoAsset
                             , _stillFrameTimeSpan :: TimeSpan
                             , _stillFrameDuration :: Duration
                             }
  deriving (Show, Eq, Generic)

makeLenses ''StillFrame

instance HasDuration VideoClip where
  durationOf mode (VideoClip _ ts speed) = case mode of
    AdjustedDuration -> durationInSpeed (durationOf OriginalDuration ts) speed
    OriginalDuration -> durationOf OriginalDuration ts

instance HasDuration StillFrame where
  durationOf _ (StillFrame _ _ _ d) = d

data CompositionPart mt where
  VideoClipPart :: VideoClip -> CompositionPart 'Video
  StillFramePart :: StillFrame -> CompositionPart 'Video
  AudioClipPart :: AudioAsset -> CompositionPart 'Audio
  SilencePart :: Duration -> CompositionPart 'Audio

makePrisms ''CompositionPart

instance HasDuration (CompositionPart mt) where
  durationOf mode = \case
    VideoClipPart vc -> durationOf mode vc
    AudioClipPart a -> durationOf mode a
    StillFramePart sf  -> durationOf mode sf
    SilencePart d -> d

deriving instance Eq (CompositionPart t)
deriving instance Show (CompositionPart t)

data Composition = Composition
  { _videoParts :: NonEmpty (CompositionPart 'Video)
  , _audioParts :: NonEmpty (CompositionPart 'Audio)
  }
  deriving (Show, Eq, Generic)

makeLenses ''Composition

instance Semigroup Composition where
  Composition v1 a1 <> Composition v2 a2 = Composition (v1 <> v2) (a1 <> a2)

instance HasDuration Composition where
  durationOf mode (Composition vs as) =
    max (foldMap (durationOf mode) vs) (foldMap (durationOf mode) as)

data Tracks = Tracks [CompositionPart 'Video] [CompositionPart 'Audio]
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
  (Composition (pure (VideoClipPart (VideoClip asset ts speed)))
               (pure (SilencePart (durationOf AdjustedDuration ts)))
  )
singleVideoPart _ = Nothing

flattenSequenceTracks :: Core.Sequence a -> Maybe Tracks
flattenSequenceTracks (Core.Sequence _ pars) = foldMap flattenParallelTracks pars

flattenParallelTracks :: Core.Parallel a -> Maybe Tracks
flattenParallelTracks (Core.Parallel _ vt at) = flattenTracks vt at

flattenTracks :: Core.VideoTrack a -> Core.AudioTrack a -> Maybe Tracks
flattenTracks (Core.VideoTrack _ vs) (Core.AudioTrack _ as) =
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
              (map (StillFramePart . StillFrame FirstFrame asset ts) precedingGaps <>
               [VideoClipPart (VideoClip asset ts speed)])
              []
          , Just (asset, ts)
          , [])
        Core.VideoGap _ d -> (tracks, lastAsset, precedingGaps <> [d])
    toAudio =
      \case
        Core.AudioClip _ asset -> Tracks [] [AudioClipPart asset]
        Core.AudioGap _ d -> Tracks [] [SilencePart d]
    matchTrackDurations video audio (lastAsset, ts) =
      case (durationOf AdjustedDuration video, durationOf AdjustedDuration audio) of
        (vd, ad)
          | vd < ad ->
            video <> Tracks [StillFramePart (StillFrame LastFrame lastAsset ts (ad - vd))] [] <>
            audio
          | vd > ad -> video <> audio <> Tracks [] [SilencePart (vd - ad)]
          | otherwise -> video <> audio
    addGapsWithLastFrame videoTrack mLastAsset gaps =
      mLastAsset >>= \(lastAsset, ts) ->
        pure (videoTrack <> Tracks (map (StillFramePart . StillFrame LastFrame lastAsset ts) gaps) [])
