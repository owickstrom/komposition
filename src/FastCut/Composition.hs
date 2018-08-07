{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module FastCut.Composition where

import           FastCut.Prelude

import           FastCut.Duration
import           FastCut.Library
import           FastCut.MediaType

type family InverseMediaType (t :: MediaType) :: MediaType where
  InverseMediaType Video = Audio
  InverseMediaType Audio = Video

data CompositionPart a (mt :: MediaType) where
  Clip :: a -> Asset mt -> CompositionPart a mt
  Gap :: a -> Duration -> CompositionPart a mt

deriving instance Eq a => Eq (CompositionPart a t)
deriving instance Show a => Show (CompositionPart a t)

setPartAnnotation :: a -> CompositionPart b t -> CompositionPart a t
setPartAnnotation a = \case
  Clip _ x -> Clip a x
  Gap _ d -> Gap a d

instance HasDuration (CompositionPart a t) where
  durationOf = \case
    Clip _ a -> durationOf a
    Gap _ d -> d

data CompositionType = TimelineType | SequenceType | ParallelType

data Composition a t where
  Timeline :: a -> NonEmpty (Composition a SequenceType) -> Composition a TimelineType
  Sequence :: a -> NonEmpty (Composition a ParallelType) -> Composition a SequenceType
  Parallel
    :: a
    -> [CompositionPart a Video]
    -> [CompositionPart a Audio]
    -> Composition a ParallelType

setCompositionAnnotation :: a -> Composition a t -> Composition a t
setCompositionAnnotation a = \case
  Timeline _ ss -> Timeline a ss
  Sequence _ ps -> Sequence a ps
  Parallel _ vs as -> Parallel a vs as

deriving instance Eq a => Eq (Composition a t)
deriving instance Show a => Show (Composition a t)

instance HasDuration (Composition a t) where
  durationOf =
    \case
      Timeline _ seqs -> foldMap durationOf seqs
      Sequence _ pars -> foldMap durationOf pars
      Parallel _ vs as -> max (foldMap durationOf vs) (foldMap durationOf as)

single :: Asset t -> Composition () ParallelType
single = \case
  v@VideoAsset{} -> Parallel () [Clip () v] []
  a@AudioAsset{} -> Parallel () [] [Clip () a]
