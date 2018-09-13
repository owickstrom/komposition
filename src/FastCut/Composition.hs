{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
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
module FastCut.Composition where

import           FastCut.Prelude

import           FastCut.Duration
import           FastCut.Library
import           FastCut.MediaType

data CompositionPart (mt :: MediaType) a where
  Clip :: a -> Asset mt -> CompositionPart mt a
  Gap :: a -> Duration -> CompositionPart mt a

deriving instance Eq a => Eq (CompositionPart t a)
deriving instance Show a => Show (CompositionPart t a)
deriving instance Functor (CompositionPart t)
deriving instance Generic (CompositionPart t a)

instance HasDuration (CompositionPart t a) where
  durationOf = \case
    Clip _ a -> durationOf a
    Gap _ d -> d

data Timeline a =
  Timeline (NonEmpty (Sequence a))
  deriving (Eq, Show, Functor, Generic)

data Sequence a =
  Sequence a
           (NonEmpty (Parallel a))
  deriving (Eq, Show, Functor, Generic)

data Parallel a =
  Parallel a
           [CompositionPart Video a]
           [CompositionPart Audio a]
  deriving (Eq, Show, Functor, Generic)

instance HasDuration (Timeline a) where
  durationOf (Timeline seqs) = foldMap durationOf seqs

instance HasDuration (Sequence a) where
  durationOf (Sequence _ pars) = foldMap durationOf pars

instance HasDuration (Parallel a) where
  durationOf (Parallel _ vs as) =
    max (foldMap durationOf vs) (foldMap durationOf as)

single :: Asset t -> Parallel ()
single = \case
  v@VideoAsset{} -> Parallel () [Clip () v] []
  a@AudioAsset{} -> Parallel () [] [Clip () a]
