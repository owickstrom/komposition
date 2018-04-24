{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module FastCut.Scene.View (SceneView, new, setScene, toWidget) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.State
import           Data.Text           (Text)
import           Data.Time.Clock     ()
import qualified GI.Gdk              as Gdk
import qualified GI.GLib.Constants   as GLib
import qualified GI.Gtk              as Gtk
import           GI.Pango.Enums      (EllipsizeMode (..))

import           FastCut.Focus
import           FastCut.Scene
import           FastCut.Sequence

data SceneView = SceneView { container :: Gtk.Box, sceneLabel :: Gtk.Label, scrollArea :: Gtk.ScrolledWindow }

toWidget :: SceneView -> Gtk.Box
toWidget = container

setWidthFromDuration :: (RealFrac a1, Gtk.IsWidget a) => a -> a1 -> IO ()
setWidthFromDuration widget duration =
  let width = fromIntegral (ceiling duration :: Int) * 50
  in  Gtk.widgetSetSizeRequest widget width (-1)

addClasses :: Gtk.IsWidget w => [Text] -> w -> IO w
addClasses cssClasses widget = do
  sc <- Gtk.widgetGetStyleContext widget
  mapM_ (Gtk.styleContextAddClass sc) cssClasses
  return widget

focusedClass :: Focused -> Text
focusedClass = \case
  Focused             -> "focused"
  TransitivelyFocused -> "transitively-focused"
  Blurred             -> "blurred"

newtype RendererState = RendererState { focusedBox :: Maybe Gtk.Box }

type Renderer a = StateT RendererState IO a


withSavedBox :: Focused -> IO Gtk.Box -> Renderer Gtk.Box
withSavedBox focused render' = do
  box <- liftIO render'
  case focused of
    Focused -> modify $ \s -> s { focusedBox = Just box }
    _       -> return ()
  return box

renderClip' focused metadata = do
  clipBox <- Gtk.boxNew Gtk.OrientationHorizontal 0
    >>= addClasses ["clip", focusedClass focused]
  label <- Gtk.labelNew (Just (clipName metadata))
  Gtk.labelSetEllipsize label EllipsizeModeEnd
  setWidthFromDuration  label (duration metadata)
  Gtk.boxPackStart clipBox label False False 0
  return clipBox

renderGap focused duration = do
  gapBox <- Gtk.boxNew Gtk.OrientationHorizontal 0
    >>= addClasses ["gap", focusedClass focused]
  label <- Gtk.labelNew (Just "")
  Gtk.boxPackStart gapBox label False False 0
  setWidthFromDuration gapBox duration
  return gapBox

renderClip :: Clip Focused t -> Renderer Gtk.Box
renderClip = \case
  VideoClip focused metadata ->
    withSavedBox focused (renderClip' focused metadata)
  AudioClip focused metadata ->
    withSavedBox focused (renderClip' focused metadata)
  VideoGap focused duration ->
    withSavedBox focused (renderGap focused duration)
  AudioGap focused duration ->
    withSavedBox focused (renderGap focused duration)

renderClipToBox :: Gtk.Box -> Clip Focused t -> Renderer ()
renderClipToBox box clip = do
  clip' <- renderClip clip
  liftIO (Gtk.boxPackStart box clip' False False 0)

renderSequence :: Sequence Focused -> Renderer Gtk.Box
renderSequence = \case
  Sequence focused sub -> do
    sequenceBox <-
      withSavedBox focused $
        Gtk.boxNew Gtk.OrientationHorizontal 0 >>= addClasses
          ["sequence", focusedClass focused]
    subBoxes <- mapM renderSequence sub
    liftIO $ forM_ subBoxes $ \box ->
      Gtk.boxPackStart sequenceBox box False False 0
    return sequenceBox
  Composition focused videoClips audioClips -> do
    compositionBox <-
      withSavedBox focused $ Gtk.boxNew Gtk.OrientationVertical 0 >>= addClasses
        ["composition", focusedClass focused]
    videoBox <- liftIO $ Gtk.boxNew Gtk.OrientationHorizontal 0 >>= addClasses
      ["video"]
    audioBox <- liftIO $ Gtk.boxNew Gtk.OrientationHorizontal 0 >>= addClasses
      ["audio"]
    liftIO $ do
      Gtk.boxPackStart compositionBox videoBox False False 0
      Gtk.boxPackStart compositionBox audioBox False False 0
    mapM_ (renderClipToBox videoBox) videoClips
    mapM_ (renderClipToBox audioBox) audioClips
    return compositionBox

new :: IO SceneView
new = do
  container <- Gtk.boxNew Gtk.OrientationVertical 0
  sceneLabel                      <- Gtk.labelNew Nothing
  scrollArea <- Gtk.scrolledWindowNew Gtk.noAdjustment Gtk.noAdjustment
  Gtk.scrolledWindowSetPolicy scrollArea
                              Gtk.PolicyTypeExternal
                              Gtk.PolicyTypeNever
  Gtk.boxPackStart container sceneLabel True  True  10
  Gtk.boxPackStart container scrollArea False False 10

  Gtk.widgetShowAll container

  return SceneView {..}

setScene :: SceneView -> Scene -> IO ()
setScene SceneView { .. } Scene { .. } = do
  Gtk.labelSetLabel sceneLabel sceneName

  -- Replace all sequence/clip elements (for now.)
  Gtk.containerForall scrollArea (Gtk.containerRemove scrollArea)
  (sequence', RendererState {..}) <- runStateT
    (renderSequence (applyFocus topSequence focus))
    RendererState {focusedBox = Nothing}
  Gtk.containerAdd scrollArea sequence'

  Gtk.widgetShowAll container

  scrollToFocused focusedBox

  where
    scrollToFocused = \case
      Just focusedBox ->
        void . forkIO $ do
          -- oh the hacks...
          threadDelay 1000
          void . Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
            adj <- Gtk.scrolledWindowGetHadjustment scrollArea
            (_, x, _) <- Gtk.widgetTranslateCoordinates focusedBox scrollArea 0 0
            Gtk.adjustmentSetValue adj (fromIntegral x - 2)
            return False
      Nothing ->
        putStrLn "No box focused."
