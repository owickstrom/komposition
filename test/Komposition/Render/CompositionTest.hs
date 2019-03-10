{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Komposition.Render.CompositionTest where

import           Komposition.Prelude
import qualified Prelude

import           Control.Lens                       hiding (at)
import           Hedgehog                           hiding (Parallel)
import qualified Hedgehog.Gen                       as Gen hiding (parallel)
import qualified Hedgehog.Range                     as Range

import           Komposition.Composition
import           Komposition.Duration
import           Komposition.Library
import qualified Komposition.Render.Composition     as Render

import qualified Komposition.Composition.Generators as Gen

hprop_flat_timeline_has_same_duration_as_hierarchical = property $ do
  timeline' <- forAll $
    Gen.timeline (Range.exponential 0 5) Gen.parallelWithClips
  let Just flat = Render.flattenTimeline timeline'
  durationOf AdjustedDuration timeline' === durationOf AdjustedDuration flat

hprop_flat_timeline_has_same_clips_as_hierarchical =
  property $ do
    timeline' <- forAll $ Gen.timeline (Range.exponential 0 5) Gen.parallelWithClips
    flat <- annotateShowId (Render.flattenTimeline timeline')

    flat ^.. _Just . Render.videoParts . each . Render._VideoClipPart
      === timelineVideoClips timeline'

    flat ^.. _Just . Render.audioParts . each . Render._AudioClipPart
      === timelineAudioClips timeline'

-- * Still frames in gaps

hprop_flat_timeline_uses_still_frame_from_single_clip = property $ do
  let genVideoTrack = do
        v1 <- Gen.videoClip
        vs <- Gen.list (Range.linear 0 1) Gen.videoPart
        pure (VideoTrack () (v1 : vs))
  timeline' <- forAll $ Gen.timeline
    (Range.exponential 0 5)
    (Parallel () <$> genVideoTrack <*> Gen.audioTrack)

  flat <- annotateShowId (Render.flattenTimeline timeline')

  flat
    ^.. ( _Just
        . Render.videoParts
        . each
        . Render._StillFramePart
        . Render.stillFrameMode
        )
    &   traverse_ (Render.LastFrame ===)

hprop_flat_timeline_uses_still_frames_from_subsequent_clips = property $ do
  -- Generate a parallel where the video track ends with a video clip,
  -- and where the audio track is shorter.
  let
    genParallel = do
      vt <-
        VideoTrack ()
          <$> (   snoc
              <$> Gen.list (Range.linear 1 10) Gen.videoPart
              <*> Gen.videoClip
              )
      at <- AudioTrack () . pure . AudioGap () <$> Gen.duration'
        (Range.linearFrac
          0
          (durationToSeconds (durationOf AdjustedDuration vt) - 0.1)
        )
      pure (Parallel () vt at)

  timeline' <- forAll $ Gen.timeline (Range.exponential 0 5) genParallel

  flat      <- annotateShowId (Render.flattenTimeline timeline')

  flat
    ^.. ( _Just
        . Render.videoParts
        . each
        . Render._StillFramePart
        . Render.stillFrameMode
        )
    &   traverse_ (Render.FirstFrame ===)

hprop_flat_timeline_uses_last_frame_for_automatic_video_padding = property $ do
  -- Generate a parallel where the video track only contains a video
  -- clip, and where the audio track is longer.
  let
    genParallel = do
      vt <- VideoTrack () . pure <$> Gen.videoClip
      at <- AudioTrack () . pure . AudioGap () <$> Gen.duration'
        (Range.linearFrac
          (durationToSeconds (durationOf AdjustedDuration vt) + 0.1)
          10
        )
      pure (Parallel () vt at)

  timeline' <- forAll $ Gen.timeline (Range.exponential 0 5) genParallel

  flat      <- annotateShowId (Render.flattenTimeline timeline')
  flat
    ^.. ( _Just
        . Render.videoParts
        . each
        . Render._StillFramePart
        . Render.stillFrameMode
        )
    &   traverse_ (Render.LastFrame ===)

-- * Flattening equivalences

hprop_flat_timeline_is_same_as_all_its_flat_sequences = property $ do
  timeline' <- forAll $ Gen.timeline (Range.exponential 0 5) Gen.parallel
  let flat = timeline' ^.. sequences . each
             & foldMap Render.flattenSequence
  Render.flattenTimeline timeline' === flat

hprop_flat_timeline_is_same_as_all_its_flat_parallels = property $ do
  timeline' <- forAll $ Gen.timeline (Range.exponential 0 5) Gen.parallel
  let flat = timeline' ^.. sequences . each . parallels . each
             & foldMap Render.flattenParallel
  Render.flattenTimeline timeline' === flat

----------------------------------------------------------------------
-- HELPERS
----------------------------------------------------------------------

timelineParallels :: Timeline a -> [Parallel a]
timelineParallels tl = do
  seq' <- toList (tl ^. sequences)
  toList (seq' ^. parallels)

timelineVideoClips :: Timeline a -> [Render.VideoClip]
timelineVideoClips tl = do
  par' <- timelineParallels tl
  par' ^. videoTrack . videoParts >>= \case
    VideoClip _ asset span speed -> pure (Render.VideoClip asset span speed)
    VideoGap{}                   -> mempty

timelineAudioClips :: Timeline a -> [AudioAsset]
timelineAudioClips tl = do
  par' <- timelineParallels tl
  par' ^. audioTrack . audioParts >>= \case
    AudioClip _ asset -> pure asset
    AudioGap{}        -> mempty

annotateShowId x = annotateShow x $> x

{-# ANN module ("HLint: ignore Use camelCase" :: Prelude.String) #-}
