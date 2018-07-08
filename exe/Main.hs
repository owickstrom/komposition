{-# LANGUAGE OverloadedStrings #-}

import           FastCut.Application
import           FastCut.Composition
import           FastCut.Project                    (Library (..), Project (..))
import qualified FastCut.UserInterface.GtkInterface as GtkInterface
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
                [gap1s, Clip video1s, gap3s]
                [Clip audio1s, Clip audio5s, Clip audio1s]
            , Parallel
                ()
                [gap3s, Clip video10s, gap1s]
                [Clip audio8s, Clip audio5s, Clip audio1s]
            ]
        ]
  , _library = Library [video1s, video1s] [audio1s, audio5s, audio8s]
  }
  where
    video1s = VideoClip () (ClipMetadata "video-1s" "/tmp/1.mp4" 1)
    video10s = VideoClip () (ClipMetadata "video-10s" "/tmp/10.mp4" 10)
    audio1s = AudioClip () (ClipMetadata "audio-1s" "/tmp/1.m4a" 1)
    audio5s = AudioClip () (ClipMetadata "audio-5s" "/tmp/5.m4a" 5)
    audio8s = AudioClip () (ClipMetadata "audio-8s" "/tmp/8.m4a" 8)
    gap1s = Gap () 1
    gap3s = Gap () 3

main :: IO ()
main = do
  cssPath <- getDataFileName "style.css"
  GtkInterface.run cssPath (fastcut initialProject)
