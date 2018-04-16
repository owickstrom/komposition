{-# LANGUAGE OverloadedStrings #-}

import           Data.GI.Base
import           Data.GI.Base.Properties
import           Data.Maybe
import           Data.Semigroup          ((<>))
import           Data.Text
import qualified Data.Text               as Text
import           Data.Word
import qualified GI.Gdk                  as Gdk
import           GI.GObject
import qualified GI.Gtk                  as Gtk
import           GI.Gtk.Objects.Button
import           GI.Gtk.Objects.Window   (windowResize)

import           FastCut.Focus
import           FastCut.Scene
import qualified FastCut.Scene.SceneView as SceneView
import           FastCut.Sequence
import           Paths_fastcut

cssPriority :: Word32
cssPriority = fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER :: Word32

main :: IO ()
main = do
  Gtk.init Nothing

  gladeFile    <- getDataFileName "gui.glade"
  builder      <- Gtk.builderNewFromFile (pack gladeFile)

  window       <- builderGetObject Gtk.Window builder "window"
  mainBox      <- builderGetObject Gtk.Box builder "main-box"
  styleContext <- Gtk.widgetGetStyleContext mainBox
  cssProvider  <- Gtk.cssProviderNew
  screen       <- maybe (fail "No screen?!") return =<< Gdk.screenGetDefault
  Gtk.cssProviderLoadFromPath cssProvider . Text.pack =<< getDataFileName
    "style.css"
  Gtk.styleContextAddProviderForScreen screen
                                       cssProvider
                                       cssPriority

  sceneView <- SceneView.render testScene
  Gtk.boxPackEnd mainBox sceneView True True 0

  window `Gtk.onWidgetDestroy` Gtk.mainQuit

  windowResize window 640 480

  Gtk.widgetShowAll window

  Gtk.main
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
    , focus = InSequenceFocus 1 (InCompositionFocus Video 0)
    }

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
