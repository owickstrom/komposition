{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

module Komposition.Project.Generators where

import           Komposition.Prelude                hiding (nonEmpty)

import           Hedgehog
import qualified Hedgehog.Gen                       as Gen hiding (parallel)
import           Hedgehog.Range

import           Komposition.Composition
import qualified Komposition.Composition.Generators as Gen
import           Komposition.Focus
import           Komposition.Library
import           Komposition.Project                (Project (..),
                                                     WithoutHistory)
import           Komposition.VideoSettings          (AllVideoSettings (..),
                                                     Resolution (..),
                                                     VideoSettings (..))

library :: (MonadGen m, GenBase m ~ Identity) => m Library
library =
    Library
    <$> Gen.list (linear 1 5) Gen.videoAsset
    <*> Gen.list (linear 1 5) Gen.audioAsset

resolution :: (MonadGen m, GenBase m ~ Identity) => m Resolution
resolution =
  Resolution
  <$> Gen.word (linear 200 2000)
  <*> Gen.word (linear 200 2000)

videoSettings :: (MonadGen m, GenBase m ~ Identity) => m VideoSettings
videoSettings =
  VideoSettings <$> Gen.word (linear 15 25) <*> resolution

projectWithTimelineAndFocus
  :: (MonadGen m, GenBase m ~ Identity)
  => m (Timeline (), SequenceFocus)
  -> m (WithoutHistory Project)
projectWithTimelineAndFocus genTimeline = do
  _projectName                <- Gen.text (linear 1 5) Gen.unicode
  (_timeline, _timelineFocus) <- genTimeline
  _library                    <- library
  _videoSettings <- AllVideoSettings <$> videoSettings <*> videoSettings
  return Project { .. }

project :: (MonadGen m, GenBase m ~ Identity) => m (WithoutHistory Project)
project = projectWithTimelineAndFocus (Gen.timelineWithFocus (linear 1 10) Gen.parallel)
