{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Komposition.VideoSpeed where

import           Komposition.Prelude

import           Control.Lens
import           Komposition.Duration

newtype VideoSpeed = VideoSpeed { _unVideoSpeed :: Double
                                -- ^ Video speed factor, where 1.0 is
                                -- normal speed, 2.0 is double speed.
                                }
  deriving (Show, Eq, Generic)

makeLenses ''VideoSpeed

durationInSpeed :: Duration -> VideoSpeed -> Duration
durationInSpeed d (VideoSpeed s) =
  durationFromSeconds (durationToSeconds d / s)
