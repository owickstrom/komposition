{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
module FastCut.TestLibrary where

import           FastCut.Prelude

import           FastCut.Composition
import           FastCut.Duration
import           FastCut.Library

video4s =
  VideoClip
    ()
    (VideoAsset (AssetMetadata "1.mp4" 4) (ProxyPath "1.proxy.mp4") Nothing)
    (TimeSpan 0 4)
    "thumb.png"
video10s =
  VideoClip
    ()
    (VideoAsset (AssetMetadata "2.mp4" 10) (ProxyPath "2.proxy.mp4") Nothing)
    (TimeSpan 0 10)
    "thumb.png"
audio1s = AudioClip () $ AudioAsset (AssetMetadata "1.m4a" 1)
audio4s = AudioClip () $ AudioAsset (AssetMetadata "2.m4a" 4)
audio10s = AudioClip () $ AudioAsset (AssetMetadata "3.m4a" 10)
videoGap1s = VideoGap () 1
videoGap3s = VideoGap () 3
audioGap1s = AudioGap () 1
audioGap3s = AudioGap () 3
parallel1 = Parallel () [videoGap1s, video4s] [audio1s]
parallel2 = Parallel () [videoGap3s, video10s] [audio4s, audio10s]

seqWithTwoParallels = Sequence () (parallel1 :| [parallel2])

timelineTwoParallels = Timeline (pure seqWithTwoParallels)
