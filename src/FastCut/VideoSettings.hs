{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module FastCut.VideoSettings where

import           FastCut.Prelude

import           Control.Lens

type FrameRate = Word

data Resolution = Resolution
  { _width  :: Word
  , _height :: Word
  } deriving (Eq, Show, Generic)

makeLenses ''Resolution

data VideoSettings = VideoSettings
  { _frameRate  :: FrameRate
  , _resolution :: Resolution
  } deriving (Eq, Show, Generic)

makeLenses ''VideoSettings
