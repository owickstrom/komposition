{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
module Komposition.Composition.Generators where

import           Komposition.Prelude            hiding (nonEmpty)

import           Control.Lens
import qualified Data.List.NonEmpty             as NonEmpty
import           Hedgehog                       hiding (Parallel (..))
import qualified Hedgehog.Gen                   as Gen
import           Hedgehog.Range

import           Komposition.Composition        hiding (audioTrack, videoTrack)
import           Komposition.Composition.Insert
import           Komposition.Duration
import           Komposition.Focus
import           Komposition.Library            hiding (assetMetadata)
import           Komposition.MediaType
import           Komposition.VideoSpeed

timeline :: (MonadGen m, GenBase m ~ Identity) => Range Int -> m (Parallel ()) -> m (Timeline ())
timeline range genParallel =
  Timeline <$> Gen.nonEmpty range (sequence' range genParallel)

sequence' :: (MonadGen m, GenBase m ~ Identity) => Range Int -> m (Parallel ()) -> m (Sequence ())
sequence' range genParallel = Sequence () <$> Gen.nonEmpty range genParallel

parallel :: (MonadGen m, GenBase m ~ Identity) => m (Parallel ())
parallel = Parallel () <$> videoTrack <*> audioTrack

videoTrack :: (MonadGen m, GenBase m ~ Identity) => m (VideoTrack ())
videoTrack = VideoTrack () <$> Gen.list (linear 1 10) videoPart

videoTrackWithInitialClip :: (MonadGen m, GenBase m ~ Identity) => Range Int -> m (VideoTrack ())
videoTrackWithInitialClip tailRange = do
  v <- videoClip
  vs <- Gen.list tailRange videoPart
  pure (VideoTrack () (v : vs))

audioTrack :: (MonadGen m, GenBase m ~ Identity) => m (AudioTrack ())
audioTrack = AudioTrack () <$> Gen.list (linear 1 10) audioPart

parallelWithClips :: (MonadGen m, GenBase m ~ Identity) => m (Parallel ())
parallelWithClips =
  parallelWithTracks
  (Gen.filter (any isClip) (Gen.list (linear 1 10) videoPart))
  (Gen.list (linear 0 10) audioPart)
  where
    isClip VideoClip{} = True
    isClip VideoGap{}  = False

parallelWithTracks
  :: (MonadGen m, GenBase m ~ Identity) => m [VideoPart ()] -> m [AudioPart ()] -> m (Parallel ())
parallelWithTracks genVideoParts genAudioParts =
  Parallel () <$> (VideoTrack () <$> genVideoParts) <*> (AudioTrack () <$> genAudioParts)

genVideoSpeed :: (MonadGen m, GenBase m ~ Identity) => m VideoSpeed
genVideoSpeed = VideoSpeed <$> Gen.double (linearFrac 0.1 2.0)

videoAsset :: (MonadGen m, GenBase m ~ Identity) => m VideoAsset
videoAsset = do
  meta  <- assetMetadata
  tp    <- TranscodedPath <$> Gen.string (linear 1 50) Gen.unicode
  pp    <- TranscodedPath <$> Gen.string (linear 1 50) Gen.unicode
  speed <- genVideoSpeed
  pure (VideoAsset meta tp pp speed Nothing)

audioAsset :: (MonadGen m, GenBase m ~ Identity) => m AudioAsset
audioAsset = AudioAsset <$> assetMetadata

videoPart :: (MonadGen m, GenBase m ~ Identity) => m (TrackPart 'Video ())
videoPart = Gen.choice [videoClip, videoGap]

videoClip :: (MonadGen m, GenBase m ~ Identity) => m (TrackPart 'Video ())
videoClip = do
      asset <- videoAsset
      let maxDuration = durationToSeconds (asset ^. videoAssetMetadata . duration)
      spanStart' <- duration' (linearFrac 0 maxDuration)
      spanEnd'   <- duration'
        (linearFrac (durationToSeconds spanStart' + 1) maxDuration)
      VideoClip () asset (TimeSpan spanStart' spanEnd') <$> genVideoSpeed

videoGap :: (MonadGen m, GenBase m ~ Identity) => m (TrackPart 'Video ())
videoGap = VideoGap () <$> duration' (linearFrac 1 10)

audioPart :: (MonadGen m, GenBase m ~ Identity) => m (TrackPart 'Audio ())
audioPart = Gen.choice [clip, gap]
  where
    clip = AudioClip () <$> audioAsset
    gap  = AudioGap () <$> duration' (linearFrac 1 10 :: Range Double)

duration' :: (MonadGen m, GenBase m ~ Identity) => Range Double -> m Duration
duration' range = durationFromSeconds <$> Gen.double range

assetMetadata :: (MonadGen m, GenBase m ~ Identity) => m AssetMetadata
assetMetadata =
  AssetMetadata
    <$> (OriginalPath <$> Gen.string (linear 1 50) Gen.unicode)
    <*> duration' (linearFrac 1 10)

-- With Focus

timelineWithFocus
  :: (MonadGen m, GenBase m ~ Identity)
  => Range Int
  -> m (Parallel ())
  -> m (Timeline (), Focus 'SequenceFocusType)
timelineWithFocus r genParallel = do
  t <- timeline r genParallel
  f <- sequenceFocus t
  pure (t, f)

sequenceFocus :: (MonadGen m, GenBase m ~ Identity) => Timeline () -> m (Focus 'SequenceFocusType)
sequenceFocus (Timeline seqs) =
  Gen.choice $ flip concatMap (zip [0 ..] (toList seqs)) $ \(i, seq') ->
    [ pure (SequenceFocus i Nothing)
    , SequenceFocus i . Just <$> parallelFocus seq'
    ]

parallelFocus :: (MonadGen m, GenBase m ~ Identity) => Sequence () -> m (Focus 'ParallelFocusType)
parallelFocus (Sequence _ pars) =
  Gen.choice
    $ flip concatMap (zip [0 ..] (toList pars))
    $ \(i, Parallel _ videoTrack' audioTrack') ->
        [ pure (ParallelFocus i Nothing)
        , ParallelFocus i . Just <$> videoTrackFocus videoTrack'
        , ParallelFocus i . Just <$> audioTrackFocus audioTrack'
        ]

videoTrackFocus :: (MonadGen m, GenBase m ~ Identity) => VideoTrack () -> m (Focus 'TrackFocusType)
videoTrackFocus (VideoTrack _ vs) =
  Gen.choice [pure (TrackFocus Video Nothing), TrackFocus Video . Just <$> clipFocus vs]

audioTrackFocus :: (MonadGen m, GenBase m ~ Identity) => AudioTrack () -> m (Focus 'TrackFocusType)
audioTrackFocus (AudioTrack _ vs) =
  Gen.choice [pure (TrackFocus Audio Nothing), TrackFocus Audio . Just <$> clipFocus vs]

clipFocus :: (MonadGen m, GenBase m ~ Identity) => [a] -> m (Focus 'ClipFocusType)
clipFocus xs = Gen.choice [ pure (ClipFocus i) | i <- [0 .. pred (length xs)] ]

insertion
  :: (MonadGen m, GenBase m ~ Identity)
  => Focus (ToFocusType Timeline)
  -> m (Insertion (), InsertPosition)
insertion = \case
  SequenceFocus _ Nothing ->
    (,)
      <$> (InsertSequence <$> sequence' (linear 1 5) parallel)
      <*> Gen.enumBounded
  SequenceFocus _ (Just (ParallelFocus _ Nothing)) ->
    (,)
      <$> Gen.choice
            [ InsertParallel <$> parallel
            , InsertVideoParts . NonEmpty.fromList <$> Gen.list (linear 1 5) videoPart
            , InsertAudioParts . NonEmpty.fromList <$> Gen.list (linear 1 5) audioPart
            ]
      <*> Gen.enumBounded
  SequenceFocus _ (Just (ParallelFocus _ (Just (TrackFocus Video _)))) ->
    (,) . InsertVideoParts . NonEmpty.fromList
    <$> Gen.list (linear 1 5) videoPart
    <*> Gen.enumBounded
  SequenceFocus _ (Just (ParallelFocus _ (Just (TrackFocus Audio _)))) ->
    (,) . InsertAudioParts . NonEmpty.fromList
    <$> Gen.list (linear 1 5) audioPart
    <*> Gen.enumBounded
