{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-} {-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Komposition.Import.Video.FFmpegTest where

import           Komposition.Prelude
import qualified Prelude

import qualified Data.List.Extra                 as List
import           Data.Massiv.Array               (Ix2 (..))
import qualified Data.Massiv.Array               as A
import           Graphics.ColorSpace
import           Hedgehog
import qualified Hedgehog.Gen                    as Gen
import qualified Hedgehog.Range                  as Range
import           Pipes                           ((>->))
import qualified Pipes
import qualified Pipes.Prelude                   as Pipes
import           Test.Tasty.Hspec

import           Komposition.Duration
import           Komposition.Import.Video.FFmpeg

colorImage :: Pixel RGB Word8 -> Timed MassivFrame
colorImage c = Timed (A.makeArray A.Par (640 :. 480) (const c)) 0

red = PixelRGB 255 0 0
green = PixelRGB 0 255 0

f1, f2 :: Timed MassivFrame
f1 = colorImage red
f2 = colorImage green

shouldClassifyAs inFrames outFrames =
  if and (zipWith eqFrame (Pipes.toList (classifyMovement 1.0 (Pipes.each inFrames))) outFrames)
     then return ()
     else expectationFailure "Classfied frames are not equal to expected frames"
  where
    eqFrame f1' f2' = untimed (unClassified f1') == untimed (unClassified f2')

spec_classifyMovement = do
  it "discards too short still section" $
    concat [[f1], replicate 5 f2, [f1]] `shouldClassifyAs`
    (Moving f1 : replicate 5 (Moving f2) ++ [Moving f1])
  it "classifies a still section" $
    concat [[f1], replicate 20 f2, [f1]] `shouldClassifyAs`
    concat [[Moving f1], replicate 20 (Still f2), [Moving f1]]

--
-- PROPERTIES
--

frameRate :: Int
frameRate = 10

newtype TestPixel = TestPixel Word8
  deriving (Eq)

instance Show TestPixel where
  show (TestPixel c) = show c

data TestFrame = TestFrame Ix2 [[TestPixel]]
  deriving (Eq, Show)

toPixel :: TestPixel -> Pixel RGB Word8
toPixel (TestPixel c) = PixelRGB c c c

toFrame :: TestFrame -> MassivFrame
toFrame (TestFrame _size pxs) =
  A.fromLists' A.Par (fmap toPixel <$> pxs)

genFrame :: MonadGen m => Ix2 -> m TestPixel -> m TestFrame
genFrame s@(width :. height) genPixel =
  TestFrame s <$> genColumns
  where
    genColumns = Gen.list (Range.singleton width) genRow
    genRow = Gen.list (Range.singleton height) genPixel

genPair :: MonadGen m => m a -> m a -> m [a]
genPair g1 g2 = (\a b -> [a, b]) <$> g1 <*> g2

data Segment a = Scene a | Pause a
  deriving (Eq, Functor)

type TestSegment = Segment [TestFrame]

instance Show (Segment [TestFrame]) where
  show (Scene frames) = "Scene (" <> show (length frames) <> ")"
  show (Pause frames) = "Pause (" <> show (length frames) <> ")"

instance Show (Segment TimeSpan) where
  show (Scene ts) = "Scene (" <> show ts <> ")"
  show (Pause ts) = "Pause (" <> show ts <> ")"

dropFirstAndLast :: [a] -> Maybe [a]
dropFirstAndLast xs
  | length xs >= 2 = pure (List.init ( List.tail xs))
  | otherwise = mzero

unwrapSegment :: Segment a -> a
unwrapSegment (Scene x) = x
unwrapSegment (Pause x) = x

countTestSegmentFrames :: [TestSegment] -> Int
countTestSegmentFrames = getSum . foldMap (pure . length . unwrapSegment)

unwrapScenes :: [Segment a] -> [a]
unwrapScenes = foldMap $ \case
  Scene x -> pure x
  Pause _ -> mempty

segmentWithDuration :: TestSegment -> Segment Duration
segmentWithDuration =
  fmap (durationFromSeconds . (/ fromIntegral frameRate) . fromIntegral . length)

segmentTimeSpans :: [Segment Duration] -> [Segment TimeSpan]
segmentTimeSpans = snd . foldl' go (Duration 0, [])
  where
    go (t, xs) x =
      let next = t <> unwrapSegment x
      in (next, xs <> [x $> TimeSpan t next])

genScene :: MonadGen m => Range Int -> Ix2 -> m TestSegment
genScene range resolution@(width :. height) = do
  len <- Gen.int range
  frames <- genPair (grayFrame light) (grayFrame medium)
  pure (Scene (take len (cycle frames)))

  where
    fraction :: Double -> Int
    fraction f = round (fromIntegral pixelCount * f)
    pixelCount = width * height
    medium     = 0x99
    light      = 0xff
    grayFrame color = genFrame
      resolution
      (Gen.frequency
        [ (fraction 0.98, pure (TestPixel color))
        , (fraction 0.02, pure (TestPixel 0))
        ]
      )

genPause :: MonadGen m => Range Int -> Ix2 -> m TestSegment
genPause range resolution = Pause
  <$> Gen.list range (genFrame resolution (pure (TestPixel dark)))
  where dark = 0x00

genSegment :: MonadGen m => Range Int -> Ix2 -> m TestSegment
genSegment segmentRange resolution =
  Gen.choice
  [genScene segmentRange resolution, genPause segmentRange resolution]

genSegments :: MonadGen m => Range Int -> Range Int -> Ix2 -> m [TestSegment]
genSegments numSegments segmentRange resolution = do
  n <- Gen.int numSegments
  pair <- genPair (genScene segmentRange resolution)
                  (genPause segmentRange resolution)
  cycle pair & take n & pure

addTimed :: [a] -> [Timed a]
addTimed = snd . foldl' go (0, [])
  where
    go (t, xs) x = (t + step, xs <> [Timed x t])
    step = 1 / fromIntegral frameRate

countSegments :: [Classified a] -> [Classified Int]
countSegments xs =
  case foldl' go (Nothing, []) xs of
    (Nothing, counts)   -> counts
    (Just last, counts) -> counts <> [last]
  where
    go (Just (Moving n), acc) (Moving _) = (Just (Moving (succ n)), acc)
    go (Just (Still n), acc) (Still _)   = (Just (Still (succ n)), acc)
    go (Just other, acc) new             = (Just (new $> 1), acc <> [other])
    go (Nothing, acc) new                = (Just (new $> 1), acc)

classifyAndCount :: Time -> [Timed TestFrame] -> [Classified Int]
classifyAndCount minStillSegmentTime timedFrames =
  countSegments
    (Pipes.toList
       (classifyMovement minStillSegmentTime (Pipes.each (map (fmap toFrame) timedFrames))))

totalClassifiedFrames :: [Classified Int] -> Int
totalClassifiedFrames counted =
  getSum (foldMap (pure . unClassified) counted)

assertStillLengthAtLeast :: MonadTest m => Time -> Classified Int -> m ()
assertStillLengthAtLeast t = \case
  Moving _ -> success
  Still c
    | c >= minFrames -> success
    | otherwise -> annotateShow c >> failure
  where
    minFrames = round (t * fromIntegral frameRate)

testSegmentsToPixelFrames :: [TestSegment] -> [Timed MassivFrame]
testSegmentsToPixelFrames = map (fmap toFrame) . addTimed . foldMap unwrapSegment

hprop_classifies_still_segments_of_min_length = withTests 100 . property $ do
  -- Generate test segments
  segments <- forAll $ genSegments (Range.linear 1 10)
                                   (Range.linear 1 (frameRate * 2))
                                   resolution
  -- Convert test segments to actual pixel frames
  let pixelFrames = testSegmentsToPixelFrames segments
      -- Run classifier on pixel frames
      counted =
        classifyMovement 2.0 (Pipes.each pixelFrames)
          & Pipes.toList
          & countSegments
  -- Sanity check: same number of frames
  countTestSegmentFrames segments === totalClassifiedFrames counted
  -- Then ignore first and last segment, and verify all other segments
  case dropFirstAndLast counted of
    Just middle -> traverse_ (assertStillLengthAtLeast 2.0) middle
    Nothing     -> success
  where resolution = 10 :. 10

hprop_classifies_same_scenes_as_input = withShrinks 50 . withTests 100 . property $ do
  -- Generate test segments
  segments <- forAll $ do
    let segmentLength = Range.linear (frameRate * 1) (frameRate * 5)
    genSegments (Range.linear 0 10) segmentLength resolution
  -- Convert test segments to timespanned ones, and actual pixel frames
  let segmentsWithTimespans = segments
                              & map segmentWithDuration
                              & segmentTimeSpans
      pixelFrames = testSegmentsToPixelFrames segments
      fullDuration = foldMap
                     (durationOf AdjustedDuration . unwrapSegment)
                     segmentsWithTimespans

  let classifiedFrames =
        Pipes.each pixelFrames
        & classifyMovement 0.8
        & Pipes.toList

  annotateShow (map (map time) classifiedFrames)

  -- Run classifier on pixel frames
  let classified =
        (Pipes.each classifiedFrames
         & classifyMovingScenes fullDuration)
        >-> Pipes.drain
        & Pipes.runEffect
        & runIdentity
  -- Check classified timespan equivalence
  unwrapScenes segmentsWithTimespans === classified

  where resolution = 10 :. 10

hprop_classifies_same_number_of_frames_as_input = withShrinks 50 . withTests 100 . property $ do
  -- Generate test segments
  segments <- forAll $ do
    let segmentLength = Range.linear (frameRate * 1) (frameRate * 5)
    genSegments (Range.linear 0 10) segmentLength resolution
  -- Convert test segments to timespanned ones, and actual pixel frames
  let pixelFrames = testSegmentsToPixelFrames segments

  let classifiedFrames =
        Pipes.each pixelFrames
        & classifyMovement 0.8
        & Pipes.toList

  length classifiedFrames === length pixelFrames

  where resolution = 10 :. 10
