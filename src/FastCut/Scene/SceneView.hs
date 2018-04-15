{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module FastCut.Scene.SceneView where

import           Control.Monad           (forM_, when)
import           Data.GI.Base
import           Data.GI.Base.Properties
import           Data.Maybe
import           Data.Semigroup          ((<>))
import           Data.Text
import qualified Data.Text               as Text
import           Data.Time.Clock         ()
import           GI.GObject
import qualified GI.Gtk                  as Gtk
import           GI.Gtk.Objects.Button
import           GI.Gtk.Objects.Window   (windowResize)
import           GI.Pango.Enums          (EllipsizeMode (..))

import           FastCut.Scene

setWidthFromDuration widget duration =
  Gtk.widgetSetSizeRequest widget (fromIntegral (ceiling duration) * 30) (-1)

addClasses :: Gtk.IsWidget w => [Text] -> w -> IO w
addClasses cssClasses widget = do
  sc <- Gtk.widgetGetStyleContext widget
  mapM_ (Gtk.styleContextAddClass sc) cssClasses
  return widget

renderClip :: Clip a -> IO Gtk.Box
renderClip = \case
  VideoClip metadata -> renderClip' metadata
  AudioClip metadata -> renderClip' metadata
  VideoGap  duration -> renderGap duration
  AudioGap  duration -> renderGap duration
 where
  renderClip' metadata = do
    clipBox <- Gtk.boxNew Gtk.OrientationHorizontal 0 >>= addClasses ["clip"]
    label   <- Gtk.labelNew (Just (clipName metadata))
    Gtk.labelSetEllipsize label EllipsizeModeEnd
    setWidthFromDuration  label (duration metadata)
    Gtk.boxPackStart clipBox label False False 0
    return clipBox
  renderGap duration = do
    gapBox <- Gtk.boxNew Gtk.OrientationHorizontal 0 >>= addClasses ["gap"]
    label  <- Gtk.labelNew (Just "")
    Gtk.boxPackStart gapBox label False False 0
    setWidthFromDuration gapBox duration
    return gapBox

renderClipToBox :: Gtk.Box -> Clip a -> IO ()
renderClipToBox box clip = do
  clip <- renderClip clip
  Gtk.boxPackStart box clip False False 0

renderSequence :: Sequence -> IO Gtk.Box
renderSequence = \case
  Sequence sub -> do
    sequenceBox <- Gtk.boxNew Gtk.OrientationHorizontal 0 >>= addClasses ["sequence"]
    subBoxes    <- mapM renderSequence sub
    forM_ subBoxes $ \box -> Gtk.boxPackStart sequenceBox box False False 0

    return sequenceBox
  Composition videoClips audioClips -> do
    compositionBox <- Gtk.boxNew Gtk.OrientationVertical 0 >>= addClasses ["composition"]
    videoBox       <- Gtk.boxNew Gtk.OrientationHorizontal 0 >>= addClasses ["video"]
    audioBox       <- Gtk.boxNew Gtk.OrientationHorizontal 0 >>= addClasses ["audio"]
    Gtk.boxPackStart compositionBox videoBox False False 0
    Gtk.boxPackStart compositionBox audioBox False False 0
    mapM_ (renderClipToBox videoBox) videoClips
    mapM_ (renderClipToBox audioBox) audioClips
    return compositionBox

render :: Scene -> IO Gtk.Box
render scene = do
  sceneBox   <- Gtk.boxNew Gtk.OrientationVertical 0
  sceneLabel <- Gtk.labelNew (Just (sceneName scene))
  sequence   <- renderSequence (topSequence scene)
  Gtk.boxPackStart sceneBox sceneLabel True  True  10
  Gtk.boxPackStart sceneBox sequence   False False 10
  return sceneBox
