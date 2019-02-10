{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
module Komposition.TestLibrary where

import           Komposition.Prelude

import           Komposition.Composition
import           Komposition.Duration
import           Komposition.Library
import           Komposition.VideoSpeed

video4s = VideoClip
  ()
  (VideoAsset (AssetMetadata (OriginalPath "1.mp4") 4)
              (TranscodedPath "1.transcoded.mp4")
              (TranscodedPath "1.proxy.mp4")
              (VideoSpeed 1.0)
              Nothing
  )
  (TimeSpan 0 4)
  (VideoSpeed 1)
video10s = VideoClip
  ()
  (VideoAsset (AssetMetadata (OriginalPath "2.mp4") 10)
              (TranscodedPath "2.transcoded.mp4")
              (TranscodedPath "2.proxy.mp4")
              (VideoSpeed 1.0)
              Nothing
  )
  (TimeSpan 0 10)
  (VideoSpeed 1)
audio1s = AudioClip () $ AudioAsset (AssetMetadata (OriginalPath "1.m4a") 1)
audio4s = AudioClip () $ AudioAsset (AssetMetadata (OriginalPath "2.m4a") 4)
audio10s = AudioClip () $ AudioAsset (AssetMetadata (OriginalPath "3.m4a") 10)
videoGap1s = VideoGap () 1
videoGap3s = VideoGap () 3
audioGap1s = AudioGap () 1
audioGap3s = AudioGap () 3
parallel1 = Parallel () (VideoTrack () [videoGap1s, video4s]) (AudioTrack () [audio1s])
parallel2 = Parallel () (VideoTrack () [videoGap3s, video10s]) (AudioTrack () [audio4s, audio10s])

seqWithTwoParallels = Sequence () (parallel1 :| [parallel2])

timelineTwoParallels = Timeline (pure seqWithTwoParallels)
