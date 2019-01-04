{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
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

type family CompositionPart (mt :: MediaType) where
  CompositionPart 'Video = VideoPart
  CompositionPart 'Audio = AudioPart

data VideoPart a
  = VideoClip a VideoAsset TimeSpan VideoSpeed FilePath
  | VideoGap a Duration
  deriving (Eq, Show, Functor, Generic)

data AudioPart a
  = AudioClip a AudioAsset
  | AudioGap a Duration
  deriving (Eq, Show, Functor, Generic)

instance HasDuration (VideoPart a) where
  durationOf mode part = case (mode, part) of
    (OriginalDuration, VideoClip _ _ ts _ _) ->
      durationOf OriginalDuration ts
    (AdjustedDuration, VideoClip _ _ ts speed _) ->
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

data Parallel a =
  Parallel a
           [VideoPart a]
           [AudioPart a]
  deriving (Eq, Show, Functor, Generic)

instance HasDuration (Timeline a) where
  durationOf mode (Timeline seqs) = foldMap (durationOf mode) seqs

instance HasDuration (Sequence a) where
  durationOf mode (Sequence _ pars) = foldMap (durationOf mode) pars

instance HasDuration (Parallel a) where
  durationOf mode (Parallel _ vs as) =
    max (foldMap (durationOf mode) vs) (foldMap (durationOf mode) as)

emptyTimeline :: Timeline ()
emptyTimeline = Timeline (Sequence () (Parallel () [] [] :| []) :| [])

data SomeComposition a
  = SomeSequence (Sequence a)
  | SomeParallel (Parallel a)
  | SomeVideoPart (CompositionPart Video a)
  | SomeAudioPart (CompositionPart Audio a)
  deriving (Show, Eq)

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
