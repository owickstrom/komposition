{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Komposition.Render.CompositionTest where

import           Komposition.Prelude
import qualified Prelude

import           Control.Lens
import           Hedgehog                           hiding (Parallel)
import qualified Hedgehog.Range                     as Range

import           Komposition.Composition
import           Komposition.Duration
import           Komposition.Library
import qualified Komposition.Render.Composition     as Render

import qualified Komposition.Composition.Generators as Gen

hprop_flat_timeline_has_same_duration_as_hierarchical =
  property $ do
    s <- forAll $ Gen.timeline (Range.linear 1 10) Gen.parallelWithClips
    let Just flat = Render.flattenTimeline s
    durationOf AdjustedDuration s === durationOf AdjustedDuration flat

hprop_flat_timeline_has_same_video_clips_as_hierarchical =
  property $ do
    s <- forAll $ Gen.timeline (Range.linear 1 10) Gen.parallelWithClips
    let Just flat = Render.flattenTimeline s
    timelineVideoClips s === flatVideoClips flat

hprop_flat_timeline_has_same_audio_clips_as_hierarchical =
  property $ do
    s <- forAll $ Gen.timeline (Range.linear 1 10) Gen.parallelWithClips
    let Just flat = Render.flattenTimeline s
    timelineAudioClips s === flatAudioClips flat

----------------------------------------------------------------------
-- HELPERS
----------------------------------------------------------------------

timelineParallels :: Timeline a -> [Parallel a]
timelineParallels tl = do
  seq' <- toList (tl ^. sequences)
  toList (seq' ^. parallels)

timelineVideoClips :: Timeline a -> [(VideoAsset, TimeSpan)]
timelineVideoClips tl = do
  par' <- timelineParallels tl
  par' ^. videoTrack . videoParts >>= \case
    VideoClip _ asset span _ -> pure (asset, span)
    VideoGap{}               -> mempty

timelineAudioClips :: Timeline a -> [AudioAsset]
timelineAudioClips tl = do
  par' <- timelineParallels tl
  par' ^. audioTrack . audioParts >>= \case
    AudioClip _ asset -> pure asset
    AudioGap{}        -> mempty

flatVideoClips :: Render.Composition -> [(VideoAsset, TimeSpan)]
flatVideoClips (Render.Composition vs _) = toList vs & foldMap (\case
  Render.VideoClip asset span _ -> pure (asset, span)
  Render.StillFrame{}           -> mempty)

flatAudioClips :: Render.Composition -> [AudioAsset]
flatAudioClips (Render.Composition _ as) = toList as & foldMap (\case
  Render.AudioClip asset -> pure asset
  Render.Silence{}       -> mempty)

{-# ANN module ("HLint: ignore Use camelCase" :: Prelude.String) #-}
