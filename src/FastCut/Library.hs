{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
module FastCut.Library where

import           FastCut.Prelude

import           Control.Lens

import           FastCut.Duration
import           FastCut.MediaType

data AssetMetadata = AssetMetadata
  { _clipName :: Text
  , _path     :: FilePath
  , _duration :: Duration
  } deriving (Eq, Show)

makeLenses ''AssetMetadata

data Asset t where
  VideoAsset :: AssetMetadata -> Asset Video
  AudioAsset :: AssetMetadata -> Asset Audio

assetMetadata ::
     Functor f => (AssetMetadata -> f AssetMetadata) -> Asset t -> f (Asset t)
assetMetadata f = \case
  VideoAsset meta -> VideoAsset <$> f meta
  AudioAsset meta -> AudioAsset <$> f meta

deriving instance Eq (Asset t)
deriving instance Show (Asset t)

instance HasDuration (Asset t) where
  durationOf = \case
    VideoAsset meta -> meta ^. duration
    AudioAsset meta -> meta ^. duration

data Library = Library
  { _videoAssets :: [Asset Video]
  , _audioAssets :: [Asset Audio]
  } deriving (Eq, Show)

makeLenses ''Library

instance Semigroup Library where
  l1 <> l2 =
    Library
    { _videoAssets = _videoAssets l1 <> _videoAssets l2
    , _audioAssets = _audioAssets l1 <> _audioAssets l2
    }

instance Monoid Library where
  mempty = Library mempty mempty
