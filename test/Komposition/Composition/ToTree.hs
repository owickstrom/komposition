{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- | In order to make timelines and composition easier to compare for
-- equality in tests they can be converted to comparable 'Tree'
-- structures, e.g. with floating-point numbers truncated to integers.
module Komposition.Composition.ToTree where

import           Komposition.Prelude

import           Control.Lens
import           Data.String
import           Data.Tree

import           Komposition.Composition
import           Komposition.Duration
import           Komposition.Library
import           Komposition.VideoSpeed

someCompositionToTree :: Show a => SomeComposition a -> Tree String
someCompositionToTree = \case
  SomeSequence s -> sequenceToTree s
  SomeParallel s -> parallelToTree s
  SomeVideoTrack t -> videoTrackToTree t
  SomeAudioTrack t -> audioTrackToTree t
  SomeVideoPart p -> videoPartToTree p
  SomeAudioPart p -> audioPartToTree p

timelineToTree :: Show a => Timeline a -> Tree String
timelineToTree (Timeline seqs) =
  Node "Timeline" (toList (sequenceToTree <$> seqs))

sequenceToTree :: Show a => Sequence a -> Tree String
sequenceToTree (Sequence ann pars) =
  Node ("Sequence " <> show ann) (toList (parallelToTree <$> pars))

parallelToTree :: Show a => Parallel a -> Tree String
parallelToTree (Parallel ann videoTrack' audioTrack') =
  Node ("Parallel " <> show ann) [videoTrackToTree videoTrack', audioTrackToTree audioTrack']

videoTrackToTree :: Show a => VideoTrack a -> Tree String
videoTrackToTree (VideoTrack ann vs) =
  Node ("Video Track " <> show ann) (videoPartToTree <$> vs)

audioTrackToTree :: Show a => AudioTrack a -> Tree String
audioTrackToTree (AudioTrack ann as) =
  Node ("Audio Track " <> show ann) (audioPartToTree <$> as)

videoPartToTree :: Show a => VideoPart a -> Tree String
videoPartToTree (VideoClip ann asset ts speed) = Node
  ("VideoClip " <> show ann)
  [videoAssetToTree asset, timeSpanToTree ts, speedToTree speed]
videoPartToTree (VideoGap ann meta)      = Node ("VideoGap " <> show ann) [Node (show meta) []]

audioPartToTree :: Show a => AudioPart a -> Tree String
audioPartToTree (AudioClip ann asset) =
  Node ("AudioClip " <> show ann) [assetMetadataToTree (asset ^. assetMetadata)]
audioPartToTree (AudioGap ann dur)  = Node ("AudioGap " <> show ann <> " " <> showTruncatedDuration dur) []


videoAssetToTree :: VideoAsset -> Tree String
videoAssetToTree asset =
  Node "VideoAsset"
  [ assetMetadataToTree (asset ^. videoAssetMetadata)
  , Node ("Transcoded: " <> show (asset ^. videoAssetTranscoded . unProxyPath)) []
  , Node ("Proxy: " <> show (asset ^. videoAssetProxy . unProxyPath)) []
  , speedToTree (asset ^. videoSpeed)
  , Node (show (asset ^. videoClassifiedScene)) []
  ]

assetMetadataToTree :: AssetMetadata -> Tree String
assetMetadataToTree meta = Node
  "AssetMetadata"
  [ Node ("Path: " <> show (meta ^. path . unOriginalPath)) []
  , Node
    (  "Duration: " <> showTruncatedDuration (meta ^. duration)
    )
    []
  ]

showTruncatedDuration :: Duration -> String
showTruncatedDuration d = show (truncate (durationToSeconds d) :: Int)

timeSpanToTree :: TimeSpan -> Tree String
timeSpanToTree (TimeSpan start end) =
  Node (showTruncatedDuration start <> "-" <> showTruncatedDuration end) []

speedToTree :: VideoSpeed -> Tree String
speedToTree speed =
  Node ("Speed: " <> toS (formatSpeed speed)) []
