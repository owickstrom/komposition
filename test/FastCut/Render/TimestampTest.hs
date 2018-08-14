{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-type-defaults #-}

module FastCut.Render.TimestampTest where

import           FastCut.Prelude
import qualified Prelude

import           Hedgehog
import           Hedgehog.Range

import           FastCut.Render.FFmpeg

import qualified FastCut.Composition.Generators as Gen

hprop_roundtripTimestamp =
  property $ do
    d <- forAll $ Gen.duration (exponential 1 (2 ^ 16))

    let printed = prettyPrintTimestamp d
    annotate (toS printed)

    let parsed = parseTimestamp printed
    annotateShow parsed

    parsed === Just d

{-# ANN module ("HLint: ignore Use camelCase" :: Prelude.String) #-}
