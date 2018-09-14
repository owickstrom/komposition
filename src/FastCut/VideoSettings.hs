{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module FastCut.VideoSettings where

import           FastCut.Prelude

import           Control.Lens

data Resolution = Resolution
  { _width  :: Integer
  , _height :: Integer
  } deriving (Eq, Show, Generic)

makeLenses ''Resolution

data VideoSettings = VideoSettings
  { _frameRate  :: Integer
  , _resolution :: Resolution
  } deriving (Eq, Show, Generic)

makeLenses ''VideoSettings
