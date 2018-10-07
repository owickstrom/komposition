{-# LANGUAGE RecordWildCards #-}

module Komposition.Project.Generators where

import           Komposition.Prelude                   hiding ( nonEmpty )

import           Hedgehog
import qualified Hedgehog.Gen                  as Gen hiding (parallel)
import           Hedgehog.Range

import           Komposition.Project (Project(..))
import           Komposition.Library
import           Komposition.VideoSettings (VideoSettings(..), Resolution(..))
import qualified Komposition.Composition.Generators as Gen

library :: MonadGen m => m Library
library =
    Library
    <$> Gen.list (linear 1 5) Gen.videoAsset
    <*> Gen.list (linear 1 5) Gen.audioAsset

resolution :: MonadGen m => m Resolution
resolution =
  Resolution
  <$> Gen.word (linear 200 2000)
  <*> Gen.word (linear 200 2000)

videoSettings :: MonadGen m => m VideoSettings
videoSettings =
  VideoSettings <$> Gen.word (linear 15 25) <*> resolution

project :: MonadGen m => m Project
project = do
  _projectName <- Gen.text (linear 1 5) Gen.unicode
  _timeline <- Gen.timeline (linear 1 10) Gen.parallel
  _library <- library
  _videoSettings <- videoSettings
  _proxyVideoSettings <- videoSettings
  return Project{..}