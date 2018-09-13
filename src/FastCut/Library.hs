{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
module FastCut.Library where

import           FastCut.Prelude

import           Control.Lens

import           FastCut.Duration
import           FastCut.MediaType

data AssetMetadata = AssetMetadata
  { _path      :: FilePath
  , _duration  :: Duration
  , _thumbnail :: Maybe FilePath
  } deriving (Eq, Show, Generic)

makeLenses ''AssetMetadata

newtype VideoAsset =
  VideoAsset AssetMetadata
  deriving (Show, Eq, Generic)

newtype AudioAsset =
  AudioAsset AssetMetadata
  deriving (Show, Eq, Generic)

type family Asset (mt :: MediaType) where
  Asset Audio = AudioAsset
  Asset Video = VideoAsset

class AssetMetadataLens a where
  assetMetadata :: Functor f => (AssetMetadata -> f AssetMetadata) -> a -> f a

instance AssetMetadataLens VideoAsset where
  assetMetadata f (VideoAsset meta) = VideoAsset <$> f meta

instance AssetMetadataLens AudioAsset where
  assetMetadata f (AudioAsset meta) = AudioAsset <$> f meta

instance HasDuration VideoAsset where
  durationOf (VideoAsset meta) = meta ^. duration

instance HasDuration AudioAsset where
  durationOf (AudioAsset meta) = meta ^. duration

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
