module FastCut.Duration where

import           FastCut.Prelude

import           Data.Time.Clock (DiffTime)

type Duration = DiffTime

class HasDuration t where
  durationOf :: t -> Duration

instance HasDuration t => HasDuration [t] where
  durationOf = foldl' (\acc c -> acc + durationOf c) 0
