{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE LambdaCase #-}
module Komposition.Composition.Generators where

import           Komposition.Prelude     hiding ( nonEmpty )

import qualified Data.List.NonEmpty                 as NonEmpty
import           Control.Lens
import           Hedgehog                hiding ( Parallel(..) )
import qualified Hedgehog.Gen                  as Gen
import           Hedgehog.Range

import           Komposition.Composition
import           Komposition.Composition.Insert
import           Komposition.Focus
import           Komposition.Duration
import           Komposition.VideoSpeed
import           Komposition.Library     hiding ( assetMetadata )
import           Komposition.MediaType

timeline :: MonadGen m => Range Int -> m (Parallel ()) -> m (Timeline ())
timeline range genParallel =
  Timeline <$> Gen.nonEmpty range (sequence' range genParallel)

sequence' :: MonadGen m => Range Int -> m (Parallel ()) -> m (Sequence ())
sequence' range genParallel = Sequence () <$> Gen.nonEmpty range genParallel

parallel :: MonadGen m => m (Parallel ())
parallel =
  Parallel ()
  <$> (VideoTrack () <$> Gen.list (linear 1 10) videoPart)
  <*> (AudioTrack () <$> Gen.list (linear 1 10) audioPart)

parallelWithClips :: MonadGen m => m (Parallel ())
parallelWithClips =
  Parallel ()
  <$> (VideoTrack () <$> Gen.filter (any isClip) (Gen.list (linear 1 10) videoPart))
  <*> (AudioTrack () <$> Gen.list (linear 0 10) audioPart)
  where
    isClip VideoClip{} = True
    isClip VideoGap{}  = False

genVideoSpeed :: MonadGen m => m VideoSpeed
genVideoSpeed = VideoSpeed <$> Gen.double (linearFrac 0.1 2.0)

videoAsset :: MonadGen m => m VideoAsset
videoAsset = do
  meta  <- assetMetadata
  tp    <- TranscodedPath <$> Gen.string (linear 1 50) Gen.unicode
  pp    <- TranscodedPath <$> Gen.string (linear 1 50) Gen.unicode
  speed <- genVideoSpeed
  pure (VideoAsset meta tp pp speed Nothing)

audioAsset :: MonadGen m => m AudioAsset
audioAsset = AudioAsset <$> assetMetadata

videoPart :: MonadGen m => m (TrackPart 'Video ())
videoPart = Gen.choice [clip, gap]
  where
    clip = do
      asset <- videoAsset
      let maxDuration =
            floor (durationToSeconds (asset ^. videoAssetMetadata . duration)) :: Int
      spanStart' <- duration' (linear 0 maxDuration)
      spanEnd'   <- duration'
        (linear (ceiling (durationToSeconds spanStart') + 1) maxDuration)
      VideoClip () asset (TimeSpan spanStart' spanEnd') <$> genVideoSpeed
    gap = VideoGap () <$> duration' (linear 1 10 :: Range Int)

audioPart :: MonadGen m => m (TrackPart 'Audio ())
audioPart = Gen.choice [clip, gap]
  where
    clip = AudioClip () <$> audioAsset
    gap  = AudioGap () <$> duration' (linear 1 10 :: Range Int)

duration' :: Integral n => MonadGen m => Range n -> m Duration
duration' range = durationFromSeconds <$> Gen.double (fromIntegral <$> range)

assetMetadata :: MonadGen m => m AssetMetadata
assetMetadata =
  AssetMetadata
    <$> (OriginalPath <$> Gen.string (linear 1 50) Gen.unicode)
    <*> duration' (linear 1 10 :: Range Int)

-- With Focus

timelineWithFocus
  :: MonadGen m
  => Range Int
  -> m (Parallel ())
  -> m (Timeline (), Focus 'SequenceFocusType)
timelineWithFocus r genParallel = do
  t <- timeline r genParallel
  f <- sequenceFocus t
  pure (t, f)

sequenceFocus :: MonadGen m => Timeline () -> m (Focus 'SequenceFocusType)
sequenceFocus (Timeline seqs) =
  Gen.choice $ flip concatMap (zip [0 ..] (toList seqs)) $ \(i, seq') ->
    [ pure (SequenceFocus i Nothing)
    , SequenceFocus i . Just <$> parallelFocus seq'
    ]

parallelFocus :: MonadGen m => Sequence () -> m (Focus 'ParallelFocusType)
parallelFocus (Sequence _ pars) =
  Gen.choice
    $ flip concatMap (zip [0 ..] (toList pars))
    $ \(i, Parallel _ videoTrack audioTrack) ->
        [ pure (ParallelFocus i Nothing)
        , ParallelFocus i . Just <$> videoTrackFocus videoTrack
        , ParallelFocus i . Just <$> audioTrackFocus audioTrack
        ]

videoTrackFocus :: MonadGen m => VideoTrack () -> m (Focus 'TrackFocusType)
videoTrackFocus (VideoTrack _ vs) = 
  Gen.choice [pure (TrackFocus Video Nothing), TrackFocus Video . Just <$> clipFocus vs]

audioTrackFocus :: MonadGen m => AudioTrack () -> m (Focus 'TrackFocusType)
audioTrackFocus (AudioTrack _ vs) =
  Gen.choice [pure (TrackFocus Audio Nothing), TrackFocus Audio . Just <$> clipFocus vs]

clipFocus :: MonadGen m => [a] -> m (Focus 'ClipFocusType)
clipFocus xs = Gen.choice [ pure (ClipFocus i) | i <- [0 .. pred (length xs)] ]

insertion
  :: MonadGen m
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
    (,)
      <$> InsertVideoParts . NonEmpty.fromList <$> Gen.list (linear 1 5) videoPart
      <*> Gen.enumBounded
  SequenceFocus _ (Just (ParallelFocus _ (Just (TrackFocus Audio _)))) ->
    (,)
      <$> InsertAudioParts . NonEmpty.fromList <$> Gen.list (linear 1 5) audioPart
      <*> Gen.enumBounded
