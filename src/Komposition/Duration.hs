{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Komposition.Duration where

import           Komposition.Prelude

import           Data.Time.Clock     (DiffTime, diffTimeToPicoseconds,
                                      picosecondsToDiffTime)

newtype Duration = Duration DiffTime
  deriving (Show, Eq, Ord, Num, Fractional, Generic)

instance Semigroup Duration where
  (Duration d1) <> (Duration d2) = Duration (d1 + d2)

instance Monoid Duration where
  mempty = 0

durationFromSeconds :: Double -> Duration
durationFromSeconds =
  Duration . picosecondsToDiffTime . round . (* 10e11)

durationToSeconds :: Duration -> Double
durationToSeconds (Duration dt) =
  fromIntegral (diffTimeToPicoseconds dt) / 10e11

data DurationMode = OriginalDuration | AdjustedDuration

class HasDuration t where
  durationOf :: DurationMode -> t -> Duration

data TimeSpan = TimeSpan
  { spanStart :: Duration
  , spanEnd   :: Duration
  } deriving (Eq, Show, Ord, Generic)

instance HasDuration TimeSpan where
  durationOf _ (TimeSpan start end) = end - start
