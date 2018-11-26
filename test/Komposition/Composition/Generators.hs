{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
module Komposition.Composition.Generators where

import           Komposition.Prelude     hiding ( nonEmpty )

import           Control.Lens
import           Hedgehog                hiding ( Parallel(..) )
import qualified Hedgehog.Gen                  as Gen
import           Hedgehog.Range

import           Komposition.Composition
import           Komposition.Focus
import           Komposition.Duration
import           Komposition.Library     hiding ( assetMetadata )
import           Komposition.MediaType

timeline :: MonadGen m => Range Int -> m (Parallel ()) -> m (Timeline ())
timeline range genParallel =
  Timeline <$> Gen.nonEmpty range (sequence' range genParallel)

sequence' :: MonadGen m => Range Int -> m (Parallel ()) -> m (Sequence ())
sequence' range genParallel = Sequence () <$> Gen.nonEmpty range genParallel

parallel :: MonadGen m => m (Parallel ())
parallel = Gen.filter notEmpty (Parallel () <$> vs <*> as)
  where
    vs = Gen.list (linear 0 10) videoPart
    as = Gen.list (linear 0 10) audioPart
    notEmpty :: Parallel () -> Bool
    notEmpty (Parallel _ vs' as') = not (null vs' && null as')

parallelWithClips :: MonadGen m => m (Parallel ())
parallelWithClips = Parallel () <$> vs <*> as
  where
    vs = Gen.filter (any isClip) (Gen.list (linear 1 10) videoPart)
    as = Gen.list (linear 0 10) audioPart
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

videoPart :: MonadGen m => m (CompositionPart Video ())
videoPart = Gen.choice [clip, gap]
  where
    clip = do
      asset <- videoAsset
      let maxDuration =
            floor (durationToSeconds (asset ^. videoAssetMetadata . duration)) :: Int
      spanStart' <- duration' (linear 0 maxDuration)
      spanEnd'   <- duration'
        (linear (ceiling (durationToSeconds spanStart') + 1) maxDuration)
      VideoClip () asset (TimeSpan spanStart' spanEnd')
        <$> Gen.string (linear 1 50) Gen.unicode
    gap = VideoGap () <$> duration' (linear 1 10 :: Range Int)

audioPart :: MonadGen m => m (CompositionPart Audio ())
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
  -> m (Timeline (), Focus SequenceFocusType)
timelineWithFocus r genParallel = do
  t <- timeline r genParallel
  f <- sequenceFocus t
  pure (t, f)

sequenceFocus :: MonadGen m => Timeline () -> m (Focus SequenceFocusType)
sequenceFocus (Timeline seqs) =
  Gen.choice $ flip concatMap (zip [0 ..] (toList seqs)) $ \(i, seq') ->
    [ pure (SequenceFocus i Nothing)
    , SequenceFocus i . Just <$> parallelFocus seq'
    ]

parallelFocus :: MonadGen m => Sequence () -> m (Focus ParallelFocusType)
parallelFocus (Sequence _ pars) =
  Gen.choice $ flip concatMap (zip [0 ..] (toList pars)) $ \(i, par) ->
    [pure (ParallelFocus i Nothing), ParallelFocus i . Just <$> clipFocus par]

clipFocus :: MonadGen m => Parallel () -> m (Focus ClipFocusType)
clipFocus (Parallel _ vs as) = Gen.choice (anyOf' Video vs <> anyOf' Audio as)
  where
    anyOf' mediaType xs =
      [ pure (ClipFocus mediaType i) | i <- [0 .. pred (length xs)] ]
