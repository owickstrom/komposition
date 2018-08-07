{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module FastCut.Render.CompositionTest where

import           FastCut.Prelude
import qualified Prelude

import           Hedgehog
import           Hedgehog.Range

import           FastCut.Duration
import qualified FastCut.Render.Composition     as Render

import qualified FastCut.Composition.Generators as Gen

hprop_flattenTimeline =
  property $ do
    s <- forAll $ Gen.timeline (linear 1 10)
    let Just flat = Render.flattenTimeline s
    durationOf s === durationOf flat

{-# ANN module ("HLint: ignore Use camelCase" :: Prelude.String) #-}
