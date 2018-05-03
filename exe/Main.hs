{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent
import           Control.Monad              (void)
import           Data.Function              ((&))
import           Data.Functor               (($>))
import           Data.GI.Base
import           Data.IORef
import           Data.Text
import qualified Data.Text                  as Text
import           Data.Word
import qualified GI.Gdk                     as Gdk
import qualified GI.GLib.Constants          as GLib
import           GI.GObject                 hiding (Object)
import qualified GI.Gtk                     as Gtk
import           GI.Gtk.Objects.Window      (windowResize)

import           FastCut.Focus
import           FastCut.Project            (Library (..), Project (..))
import qualified FastCut.Project            as Project
import           FastCut.Project.Controller (Controller)
import qualified FastCut.Project.Controller as ProjectController
import           FastCut.Sequence
import           GI.Gtk.Declarative
import           Paths_fastcut

cssPriority :: Word32
cssPriority = fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER :: Word32

addKeyboardEventHandler :: Gtk.Window -> IO (Chan ProjectController.Event)
addKeyboardEventHandler window = do
  events <- newChan
  void $ window `Gtk.onWidgetKeyPressEvent` \eventKey -> do
    keyVal <- Gdk.getEventKeyKeyval eventKey
    case keyVal of
      Gdk.KEY_Left  -> publish events (ProjectController.FocusEvent FocusLeft)
      Gdk.KEY_Right -> publish events (ProjectController.FocusEvent FocusRight)
      Gdk.KEY_Up    -> publish events (ProjectController.FocusEvent FocusUp)
      Gdk.KEY_Down  -> publish events (ProjectController.FocusEvent FocusDown)
      Gdk.KEY_l     -> publish events ProjectController.OpenLibrary
      Gdk.KEY_a     -> publish events ProjectController.Append
      Gdk.KEY_Escape     -> publish events ProjectController.Cancel
      _             -> ignore
  return events
 where
  publish events event = writeChan events event $> False
  ignore = return False

startRenderLoopBox ::
     Gtk.Box -> (m -> e -> m) -> (m -> Object) -> Chan e -> m -> IO ()
startRenderLoopBox box update render events initial = do
  let firstObj = render initial
  firstWidget <- create firstObj
  Gtk.boxPackStart box firstWidget True True 0
  Gtk.widgetShowAll box
  widget <- newIORef firstWidget
  void (forkIO (loop widget firstObj initial))
  where
    patchBox w o1 o2 =
      case patch o1 o2 of
        Modify f  -> f =<< readIORef w
        Replace createNew -> do
          Gtk.containerRemove box =<< readIORef w
          newWidget <- createNew
          writeIORef w newWidget
          Gtk.boxPackStart box newWidget True True 0
          Gtk.widgetShowAll box
        Keep      -> return ()
    loop widget oldObj model= do
      event <- readChan events
      let model' = update model event
      let newObj = render model'
      void . Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
        patchBox widget oldObj newObj
        return False
      loop widget newObj model'

startProjectRenderLoop :: Chan ProjectController.Event -> Gtk.Box -> Controller -> IO ()
startProjectRenderLoop events box =
  startRenderLoopBox
    box
    ProjectController.update
    ProjectController.render
    events

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
  void $ Gtk.init Nothing

  gladeFile   <- getDataFileName "gui.glade"
  builder     <- Gtk.builderNewFromFile (pack gladeFile)

  window      <- builderGetObject Gtk.Window builder "window"
  mainBox     <- builderGetObject Gtk.Box builder "main-box"

  cssProvider <- Gtk.cssProviderNew
  screen      <- maybe (fail "No screen?!") return =<< Gdk.screenGetDefault
  Gtk.cssProviderLoadFromPath cssProvider . Text.pack =<< getDataFileName
    "style.css"
  Gtk.styleContextAddProviderForScreen screen cssProvider cssPriority

  events <- addKeyboardEventHandler window
  startProjectRenderLoop
    events
    mainBox
    (ProjectController.makeController initialProject)

  void $ window `Gtk.onWidgetDestroy` Gtk.mainQuit

  windowResize window 640 480

  Gtk.widgetShowAll window

  Gtk.main

builderGetObject
  :: (GI.GObject.GObject b, Gtk.IsBuilder a)
  => (Data.GI.Base.ManagedPtr b -> b)
  -> a
  -> Prelude.String
  -> IO b
builderGetObject objectTypeClass builder objectId =
  Gtk.builderGetObject builder (pack objectId)
    >>= maybe
          (fail $ "Expected object with ID '" <> objectId <> "' to exist.")
          return
    >>= Gtk.unsafeCastTo objectTypeClass
