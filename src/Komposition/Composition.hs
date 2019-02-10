{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
module Komposition.Composition where

import           Komposition.Prelude

import           Control.Lens

import           Komposition.Duration
import           Komposition.Library
import           Komposition.MediaType
import           Komposition.VideoSpeed

type family TrackPart (mt :: MediaType) where
  TrackPart 'Video = VideoPart
  TrackPart 'Audio = AudioPart

data VideoPart a
  = VideoClip a VideoAsset TimeSpan VideoSpeed
  | VideoGap a Duration
  deriving (Eq, Show, Functor, Generic)

data AudioPart a
  = AudioClip a AudioAsset
  | AudioGap a Duration
  deriving (Eq, Show, Functor, Generic)

instance HasDuration (VideoPart a) where
  durationOf mode part = case (mode, part) of
    (OriginalDuration, VideoClip _ _ ts _) ->
      durationOf OriginalDuration ts
    (AdjustedDuration, VideoClip _ _ ts speed) ->
      durationInSpeed (durationOf OriginalDuration ts) speed
    (_, VideoGap _ d) -> d

instance HasDuration (AudioPart a) where
  durationOf _ = \case
    AudioClip _ a -> durationOf OriginalDuration a
    AudioGap _ d -> d

newtype Timeline a =
  Timeline (NonEmpty (Sequence a))
  deriving (Eq, Show, Functor, Generic)

data Sequence a =
  Sequence a
           (NonEmpty (Parallel a))
  deriving (Eq, Show, Functor, Generic)

data Parallel a = Parallel a (VideoTrack a) (AudioTrack a)
  deriving (Eq, Show, Functor, Generic)

data VideoTrack a = VideoTrack a [VideoPart a]
  deriving (Eq, Show, Functor, Generic)

instance Semigroup a => Semigroup (VideoTrack a) where
  VideoTrack a1 p1 <> VideoTrack a2 p2 = VideoTrack (a1 <> a2) (p1 <> p2)

instance Monoid a => Monoid (VideoTrack a) where
  mempty = VideoTrack mempty mempty

data AudioTrack a = AudioTrack a [AudioPart a]
  deriving (Eq, Show, Functor, Generic)

instance Semigroup a => Semigroup (AudioTrack a) where
  AudioTrack a1 p1 <> AudioTrack a2 p2 = AudioTrack (a1 <> a2) (p1 <> p2)

instance Monoid a => Monoid (AudioTrack a) where
  mempty = AudioTrack mempty mempty

instance HasDuration (Timeline a) where
  durationOf mode (Timeline seqs) = foldMap (durationOf mode) seqs

instance HasDuration (Sequence a) where
  durationOf mode (Sequence _ pars) = foldMap (durationOf mode) pars

instance HasDuration (Parallel a) where
  durationOf mode (Parallel _ vs as) =
    max (durationOf mode vs) (durationOf mode as)

instance HasDuration (VideoTrack a) where
  durationOf mode (VideoTrack _ parts') =
    foldMap (durationOf mode) parts'

instance HasDuration (AudioTrack a) where
  durationOf mode (AudioTrack _ parts') =
    foldMap (durationOf mode) parts'

emptyTimeline :: Timeline ()
emptyTimeline = Timeline (Sequence () (Parallel () mempty mempty:| []) :| [])

data SomeComposition a
  = SomeSequence (Sequence a)
  | SomeParallel (Parallel a)
  | SomeVideoTrack (VideoTrack a)
  | SomeAudioTrack (AudioTrack a)
  | SomeVideoPart (TrackPart 'Video a)
  | SomeAudioPart (TrackPart 'Audio a)
  deriving (Show, Eq, Functor)

-- * Lenses and prisms

_Sequence :: Prism (SomeComposition a) (SomeComposition a) (Sequence a) (Sequence a)
_Sequence = prism' SomeSequence $ \case
  SomeSequence s -> Just s
  _ -> Nothing

_Parallel :: Prism (SomeComposition a) (SomeComposition a) (Parallel a) (Parallel a)
_Parallel = prism' SomeParallel $ \case
  SomeParallel s -> Just s
  _ -> Nothing

_VideoPart :: Prism (SomeComposition a) (SomeComposition a) (VideoPart a) (VideoPart a)
_VideoPart = prism' SomeVideoPart $ \case
  SomeVideoPart s -> Just s
  _ -> Nothing

_AudioPart :: Prism (SomeComposition a) (SomeComposition a) (AudioPart a) (AudioPart a)
_AudioPart = prism' SomeAudioPart $ \case
  SomeAudioPart s -> Just s
  _ -> Nothing
