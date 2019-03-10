{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Komposition.Render.FFmpegTest where

import           Control.Effect
import           Control.Lens
import qualified Data.List.NonEmpty              as NonEmpty
import           Komposition.Prelude
import qualified Pipes.Prelude                   as Pipes
import           Pipes.Safe                      (runSafeT)
import           System.FilePath
import           System.IO.Temp
import           Test.Tasty.Hspec

import           Komposition.Duration
import           Komposition.Import.Video.FFmpeg (getVideoFileDuration,
                                                  initialize)
import           Komposition.Library
import           Komposition.Progress
import           Komposition.Render
import           Komposition.Render.Composition
import           Komposition.Render.FFmpeg
import           Komposition.VideoSettings
import           Komposition.VideoSpeed

spec_FFmpegRender :: Spec
spec_FFmpegRender =
  describe "render progress"
    $ before_ initialize
    $ around (withSystemTempDirectory "ffmpeg-render-test")
    $ do
        it "does not exceed 1.0 for video in original speed" $ \d -> do
          (clip, fullDuration) <- getClip "test/data/clip2.mp4"
          let
            speed       = VideoSpeed 1
            span        = TimeSpan 0 fullDuration
            composition = Composition (pure (VideoClipPart (VideoClip clip span speed)))
                                      (pure (SilencePart fullDuration))
          runAndCheckProgressAndDuration d composition fullDuration

        it "does not exceed 1.0 for video in double speed" $ \d -> do
          (clip, fullDuration) <- getClip "test/data/clip2.mp4"
          let speed       = VideoSpeed 2
              chunk       = fullDuration / 2
              span1       = TimeSpan 0 chunk
              span2       = TimeSpan chunk (chunk * 2)
              composition = Composition
                (VideoClipPart (VideoClip clip span1 speed) :| [VideoClipPart (VideoClip clip span2 speed)])
                (pure (SilencePart (fullDuration / 2)))
          runAndCheckProgressAndDuration d composition (fullDuration / 2)

        it "does not exceed 1.0 for video in half speed" $ \d -> do
          (clip, fullDuration) <- getClip "test/data/clip2.mp4"
          let speed       = VideoSpeed 0.5
              chunk       = fullDuration / 2
              span1       = TimeSpan 0 chunk
              span2       = TimeSpan chunk (chunk * 2)
              composition = Composition
                (VideoClipPart (VideoClip clip span1 speed) :| [VideoClipPart (VideoClip clip span2 speed)])
                (pure (SilencePart (fullDuration * 2)))
          runAndCheckProgressAndDuration d composition (fullDuration * 2)


runAndCheckProgressAndDuration d composition expectedDuration = do
  let outFile = d </> "out.mp4"
  progress <-
    renderComposition videoSettings
                      VideoTranscoded
                      (FileOutput outFile)
                      composition
    & runFFmpegRender
    & runM
  updates <- runSafeT (Pipes.toListM progress)
  let eps = 0.5 -- tolerating 0.5% diff
      exceeds u = toPercent u > (100 + eps)
  filter exceeds updates `shouldBe` []
  d' <- getVideoFileDuration outFile
  d' `closeTo` expectedDuration

getClip :: FilePath -> IO (VideoAsset, Duration)
getClip path' = do
  d <- getVideoFileDuration path'
  return (VideoAsset (AssetMetadata (OriginalPath path') d)
                      (TranscodedPath path') -- full size
                      (TranscodedPath path') -- proxy (same in test)
                      (VideoSpeed 1) -- default speed is original speed
                      Nothing -- not a classified scene
         , d)

videoSettings  = VideoSettings 25 (NonEmpty.head resolutions)

closeTo d1 d2 =
  when (abs (durationToSeconds d1 - durationToSeconds d2) > eps) $
    expectationFailure (show d1 <> " /= " <> show d2)
  where
    eps = 0.2
