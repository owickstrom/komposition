{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE TemplateHaskell #-}
module FastCut.VideoSettings where

import           FastCut.Prelude

import           Control.Lens

type FrameRate = Word

data Resolution = Resolution
  { _width  :: Word
  , _height :: Word
  } deriving (Eq, Show, Generic, Hashable)

makeLenses ''Resolution

scaleResolution :: Double -> Resolution -> Resolution
scaleResolution x (Resolution w h) =
  Resolution (round (fromIntegral w * x)) (round (fromIntegral h * x))

data VideoSettings = VideoSettings
  { _frameRate  :: FrameRate
  , _resolution :: Resolution
  } deriving (Eq, Show, Generic, Hashable)

makeLenses ''VideoSettings
