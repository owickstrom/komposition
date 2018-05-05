{-# LANGUAGE OverloadedStrings #-}

import           FastCut.Application
import           FastCut.Project                    (Library (..), Project (..))
import           FastCut.Sequence
import qualified FastCut.UserInterface.GtkInterface as GtkInterface
import           Paths_fastcut

-- addKeyboardEventHandler :: Gtk.Window -> IO (Chan ProjectController.Event)
-- addKeyboardEventHandler window = do
--   events <- newChan
--   void $ window `Gtk.onWidgetKeyPressEvent` \eventKey -> do
--     keyVal <- Gdk.getEventKeyKeyval eventKey
--     case keyVal of
--       Gdk.KEY_Left  -> publish events (ProjectController.FocusEvent FocusLeft)
--       Gdk.KEY_Right -> publish events (ProjectController.FocusEvent FocusRight)
--       Gdk.KEY_Up    -> publish events (ProjectController.FocusEvent FocusUp)
--       Gdk.KEY_Down  -> publish events (ProjectController.FocusEvent FocusDown)
--       Gdk.KEY_l     -> publish events ProjectController.OpenLibrary
--       Gdk.KEY_a     -> publish events ProjectController.Append
--       Gdk.KEY_Escape     -> publish events ProjectController.Cancel
--       _             -> ignore
--   return events
--  where
--   publish events event = writeChan events event $> False
--   ignore = return False

initialProject :: Project
initialProject = Project
  { _projectName   = "Test"
  , _topSequence = Sequence
    ()
    [ Composition () [gap1s, video1s, gap3s]  [audio1s, audio5s, audio1s]
    , Composition () [gap3s, video10s, gap1s] [audio8s, audio5s, audio1s]
    ]
  , _library = Library [video1s, video1s] [audio1s, audio5s, audio8s]
  }
 where
  video1s  = VideoClip () (ClipMetadata "video-1s" "/tmp/1.mp4" 1)
  video10s = VideoClip () (ClipMetadata "video-10s" "/tmp/10.mp4" 10)
  audio1s  = AudioClip () (ClipMetadata "audio-1s" "/tmp/1.m4a" 1)
  audio5s  = AudioClip () (ClipMetadata "audio-5s" "/tmp/5.m4a" 5)
  audio8s  = AudioClip () (ClipMetadata "audio-8s" "/tmp/8.m4a" 8)
  gap1s    = VideoGap () 1
  gap3s    = VideoGap () 3

main :: IO ()
main = do
  cssPath <- getDataFileName "style.css"
  GtkInterface.run cssPath (fastcut initialProject)
