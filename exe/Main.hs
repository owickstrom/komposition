{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent
import           Control.Monad          (void)
import           Data.GI.Base
import           Data.Semigroup         ((<>))
import           Data.Text
import qualified Data.Text              as Text
import           Data.Word
import qualified GI.Gdk                 as Gdk
import qualified GI.GLib.Constants      as GLib
import           GI.GObject
import qualified GI.Gtk                 as Gtk
import           GI.Gtk.Objects.Window  (windowResize)

import           FastCut.Focus
import           FastCut.Scene          (Scene (..), SceneView (..))
import qualified FastCut.Scene          as Scene
import qualified FastCut.Scene.Renderer as Scene
import           FastCut.Sequence
import           Paths_fastcut

cssPriority :: Word32
cssPriority = fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER :: Word32

addKeyboardEventHandler :: Gtk.Window -> IO (Chan Scene.Event)
addKeyboardEventHandler window = do
  events <- newChan
  void $ window `Gtk.onWidgetKeyPressEvent` \eventKey -> do
     keyVal <- Gdk.getEventKeyKeyval eventKey
     case keyVal of
       Gdk.KEY_Left  -> publish events (Scene.FocusEvent FocusLeft)
       Gdk.KEY_Right -> publish events (Scene.FocusEvent FocusRight)
       Gdk.KEY_Up    -> publish events (Scene.FocusEvent FocusUp)
       Gdk.KEY_Down  -> publish events (Scene.FocusEvent FocusDown)
       _             -> ignore
  return events
  where
    publish events event = writeChan events event *> return False
    ignore = return False

sceneRenderLoop :: Chan Scene.Event -> Gtk.Box -> SceneView -> IO ()
sceneRenderLoop events mainBox = loop
  where
    loop sceneView = do
      print sceneView
      widget <- Scene.render sceneView
      void . Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
        Gtk.containerForall mainBox (Gtk.containerRemove mainBox)
        Gtk.boxPackEnd mainBox widget True True 0
        Gtk.widgetShowAll widget
        return False
      event <- readChan events
      putStrLn ("Got new scene view event: " ++ show event)
      loop (Scene.update sceneView event)

initialSceneView :: SceneView
initialSceneView = SceneView
  { scene = testScene
  , focus = InSequenceFocus 0 Nothing
  }
 where
  video1    = VideoClip () (ClipMetadata "video-1" "/tmp/1.mp4" 4)
  video2    = VideoClip () (ClipMetadata "video-2" "/tmp/2.mp4" 10)
  audio1    = AudioClip () (ClipMetadata "audio-1" "/tmp/1.m4a" 5)
  audio2    = AudioClip () (ClipMetadata "audio-2" "/tmp/2.m4a" 8)
  audio3    = AudioClip () (ClipMetadata "audio-3" "/tmp/3.m4a" 5)
  gap1     = VideoGap () 1
  gap2     = VideoGap () 3
  testScene = Scene
    { sceneName = "Test"
    , topSequence = Sequence () [Composition () [gap1, video1] [audio1], Composition () [gap2, video2] [audio2, audio3]]
    }

main :: IO ()
main = do
  void $ Gtk.init Nothing

  gladeFile    <- getDataFileName "gui.glade"
  builder      <- Gtk.builderNewFromFile (pack gladeFile)

  window       <- builderGetObject Gtk.Window builder "window"
  mainBox      <- builderGetObject Gtk.Box builder "main-box"
  cssProvider  <- Gtk.cssProviderNew
  screen       <- maybe (fail "No screen?!") return =<< Gdk.screenGetDefault
  Gtk.cssProviderLoadFromPath cssProvider . Text.pack =<< getDataFileName
    "style.css"
  Gtk.styleContextAddProviderForScreen screen
                                       cssProvider
                                       cssPriority

  events <- addKeyboardEventHandler window
  void . forkIO $ sceneRenderLoop events mainBox initialSceneView

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
