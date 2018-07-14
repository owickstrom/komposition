module FastCut.Duration where

import           FastCut.Prelude

import           Data.Time.Clock (NominalDiffTime)

type Duration = NominalDiffTime

class HasDuration t where
  durationOf :: t -> Duration

instance HasDuration t => HasDuration [t] where
  durationOf = foldl' (\acc c -> acc + durationOf c) 0
