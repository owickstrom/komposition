{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module FastCut.Import.VideoTest where

import           FastCut.Prelude

import           Data.Massiv.Array    (Ix2 (..))
import qualified Data.Massiv.Array    as A
import           Graphics.ColorSpace
import qualified Pipes
import qualified Pipes.Prelude        as Pipes
import           Test.Tasty.Hspec
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen

import           FastCut.Import.Video

colorImage :: Pixel RGB Word8 -> Timed RGB8Frame
colorImage c = Timed (A.makeArray A.Par (640 :. 480) (const c)) 0

red = PixelRGB 255 0 0
green = PixelRGB 0 255 0

f1, f2 :: Timed RGB8Frame
f1 = colorImage red
f2 = colorImage green

shouldClassifyAs inFrames outFrames =
  if and (zipWith eqFrame (Pipes.toList (classifyMovement (Pipes.each inFrames))) outFrames)
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

genFrame :: MonadGen m => m (Timed RGB8Frame)

hprop_classifiesLongEnoughSegments = property $ do
  forAll $ do
    frames <- genFrame (640 :. 480)
  assert (1 == 1)
