{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
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
import qualified Pipes
import qualified Pipes.Prelude                   as Pipes
import           Test.Tasty.Hspec

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

data Segment = Scene [TestFrame] | Pause [TestFrame]

instance Show Segment where
  show (Scene frames) = "Scene (" <> show (length frames) <> ")"
  show (Pause frames) = "Pause (" <> show (length frames) <> ")"

segmentFrames :: Segment -> [TestFrame]
segmentFrames (Scene frames) = frames
segmentFrames (Pause frames) = frames

genScene :: MonadGen m => Range Int -> Ix2 -> m Segment
genScene range resolution@(width :. height) =
  Scene <$>
  Gen.list
    range
    (genFrame
       resolution
       (Gen.frequency
          [ (round @Double (fromIntegral pixelCount * 0.98), TestPixel <$> Gen.word8 (Range.linear 1 255))
          , (round @Double (fromIntegral pixelCount * 0.02), pure (TestPixel 0))
          ]))
  where
    pixelCount = width * height

genPause :: MonadGen m => Range Int -> Ix2 -> m Segment
genPause range resolution = do
  p <- TestPixel <$> Gen.word8 (Range.linear 1 255)
  Pause <$> Gen.list range (genFrame resolution (pure p))

genSegments :: MonadGen m => Range Int -> Ix2 -> m [Segment]
genSegments segmentRange resolution =
  Gen.list
    (Range.linear 1 10)
    (Gen.choice
       [genScene segmentRange resolution, genPause segmentRange resolution])

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

hprop_classifiesStillSegmentsOfMinLength =
  withTests 100 . property $ do
    segments <-
      forAll
        (genSegments
           (Range.linear 1 (frameRate * 2))
           resolution)
    let timedFrames = addTimed (foldMap segmentFrames segments)
        counted = classifyAndCount 2.0 timedFrames
    annotateShow counted
    length timedFrames === totalClassifiedFrames counted
    case List.uncons counted of
      Nothing -> failure
      Just (_first, rest) ->
        case List.unsnoc rest of
          Nothing              -> success
          Just (middle, _last) -> mapM_ (assertStillLengthAtLeast 2.0) middle
  where
    resolution = 20 :. 20
