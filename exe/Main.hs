{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent
import           Control.Monad         (void)
import           Data.Function         ((&))
import           Data.Functor          (($>))
import           Data.GI.Base
import           Data.Text
import qualified Data.Text             as Text
import           Data.Word
import qualified GI.Gdk                as Gdk
import qualified GI.GLib.Constants     as GLib
import           GI.GObject            hiding (Object)
import qualified GI.Gtk                as Gtk
import           GI.Gtk.Objects.Window (windowResize)

import           FastCut.Focus
import           FastCut.FUI
import           FastCut.Scene         (Scene (..))
import qualified FastCut.Scene         as Scene
import qualified FastCut.Scene.View    as SceneView
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
  publish events event = writeChan events event $> False
  ignore = return False

sceneRenderLoop :: Chan Scene.Event -> Gtk.Box -> Scene -> IO ()
sceneRenderLoop events container initial =
  initial
  & SceneView.renderScene
  & \case
    o@(Object first) -> do
      widget <- create first
      Gtk.boxPackEnd container widget True True 0
      Gtk.widgetShowAll container
      loop widget o initial
  where
    loop widget prevObj scene = do
      event <- readChan events
      let scene' = Scene.update scene event
      case SceneView.renderScene scene' of
        newObj -> do
          void . Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
            patchAll container [prevObj] [newObj]
            return False
          loop widget newObj scene'

initialScene :: Scene
initialScene = Scene
  { sceneName   = "Test"
  , topSequence = Sequence
    ()
    [ Composition () [gap1s, video1s, gap3s]  [audio1s, audio5s, audio1s]
    , Composition () [gap3s, video10s, gap1s] [audio8s, audio5s, audio1s]
    ]
  , focus       = InSequenceFocus 0 Nothing
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
  void . forkIO $ sceneRenderLoop events mainBox initialScene

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
