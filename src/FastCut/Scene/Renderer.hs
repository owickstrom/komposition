{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module FastCut.Scene.Renderer where

import           Control.Monad
import           Data.GI.Base
import           Data.GI.Base.Properties
import           Data.Maybe
import           Data.Semigroup          ((<>))
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Data.Time.Clock         ()
import           GI.GObject
import qualified GI.Gtk                  as Gtk
import           GI.Gtk.Objects.Button
import           GI.Gtk.Objects.Window   (windowResize)
import           GI.Pango.Enums          (EllipsizeMode (..))

import           FastCut.Focus
import           FastCut.Scene
import           FastCut.Scene.View
import           FastCut.Sequence

setWidthFromDuration :: (RealFrac a1, Gtk.IsWidget a) => a -> a1 -> IO ()
setWidthFromDuration widget duration =
  Gtk.widgetSetSizeRequest widget (fromIntegral (ceiling duration) * 30) (-1)

addClasses :: Gtk.IsWidget w => [Text] -> w -> IO w
addClasses cssClasses widget = do
  sc <- Gtk.widgetGetStyleContext widget
  mapM_ (Gtk.styleContextAddClass sc) cssClasses
  return widget

focusedClass :: Focused -> Text
focusedClass = \case
  Focused -> "focused"
  TransitivelyFocused -> "transitively-focused"
  Blurred -> "blurred"

renderClip :: Clip Focused t -> IO Gtk.Box
renderClip = \case
  VideoClip focused metadata -> renderClip' focused metadata
  AudioClip focused metadata -> renderClip' focused metadata
  VideoGap  focused duration -> renderGap focused duration
  AudioGap  focused duration -> renderGap focused duration
 where
  renderClip' focused metadata = do
    clipBox <- Gtk.boxNew Gtk.OrientationHorizontal 0 >>= addClasses ["clip", focusedClass focused]
    label   <- Gtk.labelNew (Just (clipName metadata))
    Gtk.labelSetEllipsize label EllipsizeModeEnd
    setWidthFromDuration  label (duration metadata)
    Gtk.boxPackStart clipBox label False False 0
    return clipBox
  renderGap focused duration = do
    gapBox <- Gtk.boxNew Gtk.OrientationHorizontal 0 >>= addClasses ["gap", focusedClass focused]
    label  <- Gtk.labelNew (Just "")
    Gtk.boxPackStart gapBox label False False 0
    setWidthFromDuration gapBox duration
    return gapBox

renderClipToBox :: Gtk.Box -> Clip Focused t -> IO ()
renderClipToBox box clip = do
  clip <- renderClip clip
  Gtk.boxPackStart box clip False False 0

renderSequence :: Sequence Focused -> IO Gtk.Box
renderSequence = \case
  Sequence focused sub -> do
    sequenceBox <- Gtk.boxNew Gtk.OrientationHorizontal 0 >>= addClasses ["sequence", focusedClass focused]
    subBoxes    <- mapM renderSequence sub
    forM_ subBoxes $ \box -> Gtk.boxPackStart sequenceBox box False False 0
    return sequenceBox
  Composition focused videoClips audioClips -> do
    compositionBox <- Gtk.boxNew Gtk.OrientationVertical 0 >>= addClasses ["composition", focusedClass focused]
    videoBox       <- Gtk.boxNew Gtk.OrientationHorizontal 0 >>= addClasses ["video"]
    audioBox       <- Gtk.boxNew Gtk.OrientationHorizontal 0 >>= addClasses ["audio"]
    Gtk.boxPackStart compositionBox videoBox False False 0
    Gtk.boxPackStart compositionBox audioBox False False 0
    mapM_ (renderClipToBox videoBox) videoClips
    mapM_ (renderClipToBox audioBox) audioClips
    return compositionBox

render :: SceneView -> IO Gtk.Box
render SceneView { scene, focus } = do
  sceneBox   <- Gtk.boxNew Gtk.OrientationVertical 0
  sceneLabel <- Gtk.labelNew (Just (sceneName scene))
  sequence   <- renderSequence (applyFocus (topSequence scene) focus)
  Gtk.boxPackStart sceneBox sceneLabel True  True  10
  Gtk.boxPackStart sceneBox sequence   False False 10
  return sceneBox
