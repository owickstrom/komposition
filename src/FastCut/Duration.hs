{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FastCut.Duration where

import           FastCut.Prelude

import           Data.Time.Clock (DiffTime, diffTimeToPicoseconds,
                                  picosecondsToDiffTime)
import           Dhall

newtype Duration = Duration DiffTime
  deriving (Show, Eq, Ord, Num, Generic)

instance Semigroup Duration where
  (Duration d1) <> (Duration d2) = Duration (d1 + d2)

instance Monoid Duration where
  mempty = 0

instance Interpret Duration where
  autoWith _ = durationFromSeconds <$> double

durationFromSeconds :: Double -> Duration
durationFromSeconds =
  Duration . picosecondsToDiffTime . round . (* 10e11)

durationToSeconds :: Duration -> Double
durationToSeconds (Duration dt) =
  fromIntegral (diffTimeToPicoseconds dt) / 10e11

class HasDuration t where
  durationOf :: t -> Duration

data TimeSpan = TimeSpan
  { spanStart :: DiffTime
  , spanEnd   :: DiffTime
  } deriving (Eq, Show)
