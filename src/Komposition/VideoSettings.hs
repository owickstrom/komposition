{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Komposition.VideoSettings where

import           Komposition.Prelude

import           Control.Lens

type FrameRate = Word

data Resolution = Resolution
  { _width  :: Word
  , _height :: Word
  } deriving (Eq, Show, Generic, Hashable)

makeLenses ''Resolution

prettyPrintResolution :: Resolution -> Text
prettyPrintResolution (Resolution w h) =
  show w <> "x" <> show h

resolutions :: NonEmpty Resolution
resolutions =
  Resolution 1280 720
  :| [ Resolution 1920 1080
     , Resolution 2560 1440
     , Resolution 3840 2160]

data VideoSettings = VideoSettings
  { _frameRate  :: FrameRate
  , _resolution :: Resolution
  } deriving (Eq, Show, Generic, Hashable)

makeLenses ''VideoSettings

data AllVideoSettings = AllVideoSettings
  { _renderVideoSettings :: VideoSettings
  , _proxyVideoSettings  :: VideoSettings
  } deriving (Eq, Show, Generic, Hashable)

makeLenses ''AllVideoSettings
