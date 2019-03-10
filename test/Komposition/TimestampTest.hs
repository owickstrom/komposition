{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-type-defaults #-}

module Komposition.TimestampTest where

import           Komposition.Prelude
import qualified Prelude

import           Hedgehog
import           Hedgehog.Range

import           Komposition.Duration
import           Komposition.Timestamp

import qualified Komposition.Composition.Generators as Gen

equalishTo d1 d2 =
  when (abs (durationToSeconds d1 - durationToSeconds d2) > eps) $ do
    footnoteShow d1
    footnoteShow d2
    failure
  where
    eps = 0.01

hprop_roundtripTimestamp =
  property $ do
    d <- forAll $ Gen.duration' (exponentialFloat 1 (2 ^ 16))

    let printed = printTimestamp d
    annotate (toS printed)

    let Just parsed = parseTimestamp printed
    annotateShow parsed

    parsed `equalishTo` d

{-# ANN module ("HLint: ignore Use camelCase" :: Prelude.String) #-}
