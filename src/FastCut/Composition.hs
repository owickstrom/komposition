{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
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

type family InverseMediaType (t :: MediaType) :: MediaType where
  InverseMediaType Video = Audio
  InverseMediaType Audio = Video

data CompositionPart (mt :: MediaType) a where
  Clip :: a -> Asset mt -> CompositionPart mt a
  Gap :: a -> Duration -> CompositionPart mt a

deriving instance Eq a => Eq (CompositionPart t a)
deriving instance Show a => Show (CompositionPart t a)
deriving instance Functor (CompositionPart t)

setPartAnnotation :: a -> CompositionPart t b -> CompositionPart t a
setPartAnnotation a = \case
  Clip _ x -> Clip a x
  Gap _ d -> Gap a d

instance HasDuration (CompositionPart t a) where
  durationOf = \case
    Clip _ a -> durationOf a
    Gap _ d -> d

data CompositionType
  = TimelineType
  | SequenceType
  | ParallelType

data Composition (t :: CompositionType) a where
  Timeline :: NonEmpty (Composition SequenceType a) -> Composition TimelineType a
  Sequence :: a -> NonEmpty (Composition ParallelType a) -> Composition SequenceType a
  Parallel
    :: a
    -> [CompositionPart Video a]
    -> [CompositionPart Audio a]
    -> Composition ParallelType a

setCompositionAnnotation :: a -> Composition t a -> Composition t a
setCompositionAnnotation a = \case
  Timeline ss -> Timeline ss
  Sequence _ ps -> Sequence a ps
  Parallel _ vs as -> Parallel a vs as

deriving instance Eq a => Eq (Composition t a)
deriving instance Show a => Show (Composition t a)
deriving instance Functor (Composition t)

instance HasDuration (Composition t a) where
  durationOf =
    \case
      Timeline seqs -> foldMap durationOf seqs
      Sequence _ pars -> foldMap durationOf pars
      Parallel _ vs as -> max (foldMap durationOf vs) (foldMap durationOf as)

single :: Asset t -> Composition ParallelType ()
single = \case
  v@VideoAsset{} -> Parallel () [Clip () v] []
  a@AudioAsset{} -> Parallel () [] [Clip () a]
