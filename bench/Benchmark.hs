{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Komposition.Prelude

import qualified Codec.Picture                   as Juicy
import           Criterion.Main
import qualified Data.ByteString                 as ByteString
import qualified Data.Massiv.Array.IO            as Massiv
import           Komposition.Composition
import           Komposition.Composition.Focused
import           Komposition.Composition.Insert
import           Komposition.Duration
import           Komposition.Focus
import           Komposition.Library
import           Komposition.MediaType
import           Komposition.VideoSpeed

import           Komposition.Import.Video.FFmpeg

testImageName :: Int -> FilePath
testImageName n = "bench/images/" <> show n <> ".png"

readJuicyTestImage :: Int -> IO (Juicy.Image Juicy.PixelRGB8)
readJuicyTestImage n = do
  result <- Juicy.decodePng <$> ByteString.readFile (testImageName n)
  case result of
    Left err                    -> panic (toS err)
    Right (Juicy.ImageRGB8 img) -> return img
    Right _                     -> panic "Unexpected image type."

readMassivTestImage :: Int -> IO MassivFrame
readMassivTestImage = Massiv.readImage . testImageName

singleParallelTimelineWithClips :: Int -> Timeline ()
singleParallelTimelineWithClips n = Timeline
  (pure (Sequence () (pure (Parallel () (VideoTrack () (replicate n videoClip)) mempty))))

videoClip :: VideoPart ()
videoClip = VideoClip
    ()
    (VideoAsset (AssetMetadata (OriginalPath "1.mp4") 4)
                (TranscodedPath "1.transcoded.mp4")
                (TranscodedPath "1.proxy.mp4")
                (VideoSpeed 1.0)
                Nothing
    )
    (TimeSpan 0 4)
    (VideoSpeed 1)

clipFocus :: MediaType -> Int -> Focus 'SequenceFocusType
clipFocus mt i =
  SequenceFocus 0 (Just (ParallelFocus 0 (Just (TrackFocus mt (Just (ClipFocus i))))))


main :: IO ()
main = do
  himg1 <- readMassivTestImage 1
  himg2 <- readMassivTestImage 2
  defaultMain
    [ bgroup
      "frame equality check"
      [ bench "equalFrame(1)" $ whnf (equalFrame 1 0.999 himg1) himg1
      , bench "!equalFrame(1)" $ whnf (equalFrame 1 0.999 himg1) himg2
      , bench "equalFrame'(32)" $ whnf (equalFrame' 1 0.99 himg1) himg1
      , bench "!equalFrame'(32)" $ whnf (equalFrame' 1 0.99 himg1) himg2
      ]
    , bgroup
      "timeline insertion"
      [ bench "at beginning" $ whnf
        (insert_ (clipFocus Video 0) (InsertVideoParts (pure videoClip)) LeftOf)
        (singleParallelTimelineWithClips 10000)
      , bench "at end" $ whnf
        (insert_ (clipFocus Video 9999) (InsertVideoParts (pure videoClip)) RightOf)
        (singleParallelTimelineWithClips 10000)
      ]
    , bgroup
      "withAllFoci"
      [ bench "10000" $ whnf
        withAllFoci
        (singleParallelTimelineWithClips 10000)
      ]
    ]
