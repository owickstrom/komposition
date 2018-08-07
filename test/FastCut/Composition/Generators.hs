{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
module FastCut.Composition.Generators where

import           FastCut.Prelude     hiding (nonEmpty)

import           Hedgehog            hiding (Parallel (..))
import qualified Hedgehog.Gen        as Gen

import           FastCut.Composition
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

duration :: MonadGen m => Range Int -> m Duration
duration range = fromIntegral <$> Gen.int range

assetMetadata :: MonadGen m => Range Int -> m AssetMetadata
assetMetadata range =
  AssetMetadata <$> Gen.string range Gen.unicode <*> duration range <*> Gen.string range Gen.unicode
