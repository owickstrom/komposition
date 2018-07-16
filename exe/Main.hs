{-# LANGUAGE OverloadedStrings #-}

import           FastCut.Prelude

import           FastCut.Application
import           FastCut.Composition
import           FastCut.Library
import           FastCut.Project                    (Project (..))
import           FastCut.UserInterface.GtkInterface
import           FastCut.Video.FFmpeg
import           Paths_fastcut


initialProject :: Project
initialProject =
  Project
  { _projectName = "Test"
  , _timeline =
      Timeline
        ()
        [ Sequence
            ()
            [ Parallel
                ()
                [gap1s, Clip () video1s, gap3s]
                [ Clip () audio1s
                , Clip () audio5s
                , Clip () audio1s
                ]
            , Parallel
                ()
                [gap3s, Clip () video10s, gap1s]
                [ Clip () audio8s
                , Clip () audio5s
                , Clip () audio1s
                ]
            ]
        ]
  , _library = Library [video1s, video1s] [audio1s, audio5s, audio8s]
  }
  where
    video1s = VideoAsset (AssetMetadata "1.mp4" 1)
    video10s = VideoAsset (AssetMetadata "10.mp4" 10)
    audio1s = AudioAsset (AssetMetadata "1.m4a" 1)
    audio5s = AudioAsset (AssetMetadata "5.m4a" 5)
    audio8s = AudioAsset (AssetMetadata "8.m4a" 8)
    gap1s = Gap () 1
    gap3s = Gap () 3

main :: IO ()
main = do
  initialize
  cssPath <- getDataFileName "style.css"
  withGtkUserInterface cssPath runFFmpegImporterT (fastcut initialProject)
