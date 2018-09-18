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
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
module FastCut.Composition where

import           FastCut.Prelude

import           FastCut.Duration
import           FastCut.Library
import           FastCut.MediaType

type family CompositionPart (mt :: MediaType) where
  CompositionPart 'Video = VideoPart
  CompositionPart 'Audio = AudioPart

data VideoPart a
  = VideoClip a VideoAsset TimeSpan
  | VideoGap a Duration
  deriving (Eq, Show, Functor, Generic)

data AudioPart a
  = AudioClip a AudioAsset
  | AudioGap a Duration
  deriving (Eq, Show, Functor, Generic)

instance HasDuration (VideoPart a) where
  durationOf = \case
    VideoClip _ _ ts -> durationOf ts
    VideoGap _ d -> d

instance HasDuration (AudioPart a) where
  durationOf = \case
    AudioClip _ a -> durationOf a
    AudioGap _ d -> d

data Timeline a =
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
