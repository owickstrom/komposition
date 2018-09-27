{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE TemplateHaskell #-}
module Komposition.VideoSettings where

import           Komposition.Prelude

import           Control.Lens

type FrameRate = Word

data Resolution = Resolution
  { _width  :: Word
  , _height :: Word
  } deriving (Eq, Show, Generic, Hashable)

makeLenses ''Resolution

data VideoSettings = VideoSettings
  { _frameRate  :: FrameRate
  , _resolution :: Resolution
  } deriving (Eq, Show, Generic, Hashable)

makeLenses ''VideoSettings
