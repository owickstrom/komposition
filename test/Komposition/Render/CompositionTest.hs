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
import qualified Hedgehog.Gen                       as Gen
import qualified Hedgehog.Range                     as Range

import           Komposition.Composition
import           Komposition.Duration
import           Komposition.Library
import qualified Komposition.Render.Composition     as Render

import qualified Komposition.Composition.Generators as Gen

hprop_flat_timeline_has_same_duration_as_hierarchical =
  property $ do
    s <- forAll $ Gen.timeline (Range.exponential 0 5) Gen.parallelWithClips
    let Just flat = Render.flattenTimeline s
    durationOf AdjustedDuration s === durationOf AdjustedDuration flat

hprop_flat_timeline_has_same_clips_as_hierarchical =
  property $ do
    s <- forAll $ Gen.timeline (Range.exponential 0 5) Gen.parallelWithClips
    let Just flat = Render.flattenTimeline s
    timelineVideoClips s === flatVideoClips flat
    timelineAudioClips s === flatAudioClips flat

hprop_flat_timeline_uses_still_frame_from_single_clip = property $ do
  let genVideoTrack = do
        v1 <- Gen.videoClip
        vs <- Gen.list (Range.linear 0 1) Gen.videoPart
        pure (VideoTrack () (v1 : vs))
  s <- forAll $ Gen.timeline
    (Range.exponential 0 5)
    (Parallel () <$> genVideoTrack <*> Gen.audioTrack)

  flat <- annotateShowId (Render.flattenTimeline s)

  flat
    ^.. (_Just . Render.videoParts . each . Render._StillFramePart . Render.stillFrameMode)
    & traverse_ (Render.LastFrame ===)

hprop_flat_timeline_uses_still_frames_from_subsequent_clips = property $ do
  -- Generate a parallel where the video track ends with a video clip,
  -- and where the audio track is shorter.
  let
    genParallel = do
      vt <-
        VideoTrack ()
          <$> (   snoc
              <$> Gen.list (Range.linear 1 5) Gen.videoPart
              <*> Gen.videoClip
              )
      pure $ Parallel
        ()
        vt
        (AudioTrack () [AudioGap () (durationOf AdjustedDuration vt / 2)])

  s    <- forAll $ Gen.timeline (Range.exponential 0 5) genParallel

  flat <- annotateShowId (Render.flattenTimeline s)

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
      pure $ Parallel
        ()
        vt
        (AudioTrack () [AudioGap () (durationOf AdjustedDuration vt * 2)])

  s    <- forAll $ Gen.timeline (Range.exponential 0 5) genParallel

  flat <- annotateShowId (Render.flattenTimeline s)

  flat
    ^.. ( _Just
        . Render.videoParts
        . each
        . Render._StillFramePart
        . Render.stillFrameMode
        )
    &   traverse_ (Render.LastFrame ===)

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
flatVideoClips (Render.Composition vs _) = toList vs & foldMap
  (\case
    Render.VideoClipPart (Render.VideoClip asset span _) -> pure (asset, span)
    Render.StillFramePart{}                              -> mempty
  )

flatAudioClips :: Render.Composition -> [AudioAsset]
flatAudioClips (Render.Composition _ as) = toList as & foldMap (\case
  Render.AudioClipPart asset -> pure asset
  Render.SilencePart{}       -> mempty)

annotateShowId x = annotateShow x $> x

{-# ANN module ("HLint: ignore Use camelCase" :: Prelude.String) #-}
