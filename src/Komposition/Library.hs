{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
module Komposition.Library where

import           Komposition.Prelude

import           Control.Lens

import           Komposition.Duration
import           Komposition.MediaType

newtype OriginalPath = OriginalPath
  { _unOriginalPath :: FilePath
  } deriving (Show, Eq, Generic)

makeLenses ''OriginalPath

data AssetMetadata = AssetMetadata
  { _path     :: OriginalPath
  , _duration :: Duration
  } deriving (Eq, Show, Generic)

makeLenses ''AssetMetadata

newtype TranscodedPath = TranscodedPath
  { _unProxyPath :: FilePath
  } deriving (Show, Eq, Generic)

makeLenses ''TranscodedPath

newtype VideoSpeed = VideoSpeed { _unVideoSpeed :: Double
                                -- ^ Video speed factor, where 1.0 is normal speed.
                                }
  deriving (Show, Eq, Generic)

makeLenses ''VideoSpeed

data VideoAsset =
  VideoAsset { _videoAssetMetadata   :: AssetMetadata
             , _videoAssetTranscoded :: TranscodedPath
             , _videoAssetProxy      :: TranscodedPath
             , _videoSpeed           :: VideoSpeed
             , _videoClassifiedScene :: Maybe (Integer, TimeSpan)
             }
  deriving (Show, Eq, Generic)

makeLenses ''VideoAsset

newtype AudioAsset =
  AudioAsset { _audioAssetMetadata :: AssetMetadata }
  deriving (Show, Eq, Generic)

makeLenses ''AudioAsset

type family Asset (mt :: MediaType) where
  Asset Audio = AudioAsset
  Asset Video = VideoAsset

class AssetMetadataLens a where
  assetMetadata :: Functor f => (AssetMetadata -> f AssetMetadata) -> a -> f a

instance AssetMetadataLens VideoAsset where
  assetMetadata = videoAssetMetadata

instance AssetMetadataLens AudioAsset where
  assetMetadata = audioAssetMetadata

instance HasDuration VideoAsset where
  durationOf va = va ^. videoAssetMetadata . duration

instance HasDuration AudioAsset where
  durationOf aa = aa ^. audioAssetMetadata . duration

data Library = Library
  { _videoAssets :: [VideoAsset]
  , _audioAssets :: [AudioAsset]
  } deriving (Eq, Show, Generic)

makeLenses ''Library

instance Semigroup Library where
  l1 <> l2 =
    Library
    { _videoAssets = _videoAssets l1 <> _videoAssets l2
    , _audioAssets = _audioAssets l1 <> _audioAssets l2
    }

instance Monoid Library where
  mempty = Library mempty mempty
