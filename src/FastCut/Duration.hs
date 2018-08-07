{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FastCut.Duration where

import           FastCut.Prelude

import           Data.Time.Clock (DiffTime)

newtype Duration = Duration DiffTime
  deriving (Show, Eq, Ord, Num)

instance Semigroup Duration where
  (Duration d1) <> (Duration d2) = Duration (d1 + d2)

instance Monoid Duration where
  mempty = 0

class HasDuration t where
  durationOf :: t -> Duration

data TimeSpan = TimeSpan
  { spanStart :: DiffTime
  , spanEnd   :: DiffTime
  } deriving (Eq, Show)
