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
  { clipName :: Text
  , path     :: FilePath
  , duration :: Duration
  } deriving (Eq, Show)

data Asset t where
  VideoAsset :: AssetMetadata -> Asset Video
  AudioAsset :: AssetMetadata -> Asset Audio

deriving instance Eq (Asset t)
deriving instance Show (Asset t)

instance HasDuration (Asset t) where
  durationOf = \case
    VideoAsset meta -> duration meta
    AudioAsset meta -> duration meta

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
