{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Komposition.Render.CompositionTest where

import           Komposition.Prelude
import qualified Prelude

import           Hedgehog
import           Hedgehog.Range

import           Komposition.Duration
import qualified Komposition.Render.Composition     as Render

import qualified Komposition.Composition.Generators as Gen

hprop_flattenTimeline =
  property $ do
    s <- forAll $ Gen.timeline (linear 1 10) Gen.parallelWithClips
    let Just flat = Render.flattenTimeline s
    durationOf AdjustedDuration s === durationOf AdjustedDuration flat

{-# ANN module ("HLint: ignore Use camelCase" :: Prelude.String) #-}
