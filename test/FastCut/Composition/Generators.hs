{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
module FastCut.Composition.Generators where

import           FastCut.Prelude     hiding (nonEmpty)

import           Hedgehog            hiding (Parallel (..))
import qualified Hedgehog.Gen        as Gen

import           FastCut.Composition
import           FastCut.Focus
import           FastCut.Duration
import           FastCut.Library     hiding (assetMetadata, duration)
import           FastCut.MediaType

timeline ::
     MonadGen m
  => Range Int
  -> m (Composition () TimelineType)
timeline range = Timeline () <$> Gen.nonEmpty range (sequence' range)

sequence' ::
     MonadGen m
  => Range Int
  -> m (Composition () SequenceType)
sequence' range = Sequence () <$> Gen.nonEmpty range (parallel range)

parallel ::
     MonadGen m
  => Range Int
  -> m (Composition () ParallelType)
parallel range =
  Gen.filter notEmpty (Parallel () <$> vs <*> as)
  where
    vs = Gen.filter (any isClip) (Gen.list range (videoPart range))
    as = Gen.list range (audioPart range)
    isClip Clip{} = True
    isClip Gap{}  = False
    notEmpty :: Composition () ParallelType -> Bool
    notEmpty (Parallel _ vs' as') = not (null vs' && null as')

videoPart ::
     MonadGen m
  => Range Int
  -> m (CompositionPart () Video)
videoPart range =
  Gen.choice
  [Clip () . VideoAsset <$> assetMetadata range
  , Gap () <$> duration range
  ]

audioPart ::
     MonadGen m
  => Range Int
  -> m (CompositionPart () Audio)
audioPart range =
  Gen.choice
  [Clip () . AudioAsset <$> assetMetadata range
  , Gap () <$> duration range
  ]

duration :: Integral n => MonadGen m => Range n -> m Duration
duration range = durationFromSeconds <$> Gen.double (fromIntegral <$> range)

assetMetadata :: MonadGen m => Range Int -> m AssetMetadata
assetMetadata range =
  AssetMetadata <$> Gen.string range Gen.unicode <*> duration range <*> Gen.string range Gen.unicode

-- With Focus

timelineWithFocus ::
     MonadGen m
  => Range Int
  -> m (Composition () TimelineType, Focus SequenceFocusType)
timelineWithFocus r = do
  t <- timeline r
  f <- sequenceFocus t
  pure (t, f)

sequenceFocus :: MonadGen m => Composition () TimelineType -> m (Focus SequenceFocusType)
sequenceFocus (Timeline _ seqs) =
  Gen.choice $ flip concatMap (zip [0..] (toList seqs)) $ \(i, seq') ->
    [ pure (SequenceFocus i Nothing)
    , SequenceFocus i . Just <$> parallelFocus seq'
    ]

parallelFocus :: MonadGen m => Composition () SequenceType -> m (Focus ParallelFocusType)
parallelFocus (Sequence _ pars) =
  Gen.choice $ flip concatMap (zip [0..] (toList pars)) $ \(i, par) ->
    [ pure (ParallelFocus i Nothing)
    , ParallelFocus i . Just <$> clipFocus par
    ]

clipFocus :: MonadGen m => Composition () ParallelType -> m (Focus ClipFocusType)
clipFocus (Parallel _ vs as) =
  Gen.choice (anyOf Video vs <> anyOf Audio as)
  where
    anyOf mediaType xs =
      [pure (ClipFocus mediaType i) | i <- [0 .. pred (length xs)]]