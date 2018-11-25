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

import           Komposition.Duration
import           Komposition.Library
import           Komposition.MediaType

type family CompositionPart (mt :: MediaType) where
  CompositionPart 'Video = VideoPart
  CompositionPart 'Audio = AudioPart

data VideoPart a
  = VideoClip a VideoAsset TimeSpan FilePath
  | VideoGap a Duration
  deriving (Eq, Show, Functor, Generic)

data AudioPart a
  = AudioClip a AudioAsset
  | AudioGap a Duration
  deriving (Eq, Show, Functor, Generic)

instance HasDuration (VideoPart a) where
  durationOf = \case
    VideoClip _ _ ts _ -> durationOf ts
    VideoGap _ d -> d

instance HasDuration (AudioPart a) where
  durationOf = \case
    AudioClip _ a -> durationOf a
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
  durationOf (Timeline seqs) = foldMap durationOf seqs

instance HasDuration (Sequence a) where
  durationOf (Sequence _ pars) = foldMap durationOf pars

instance HasDuration (Parallel a) where
  durationOf (Parallel _ vs as) =
    max (foldMap durationOf vs) (foldMap durationOf as)

emptyTimeline :: Timeline ()
emptyTimeline = Timeline (Sequence () (Parallel () [] [] :| []) :| [])

data SomeComposition a
  = SomeSequence (Sequence a)
  | SomeParallel (Parallel a)
  | SomeVideoPart (CompositionPart Video a)
  | SomeAudioPart (CompositionPart Audio a)
  deriving (Show, Eq)
