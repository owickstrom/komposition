{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module FastCut.Scene.View (renderScene) where

import           Data.Text             (Text)
import           Data.Time.Clock       (NominalDiffTime)

import           FastCut.Focus
import           FastCut.Scene
import           FastCut.Sequence
import           FastCut.VirtualWidget

{-setWidthFromDuration :: (RealFrac a1, Gtk.IsWidget a) => a -> a1 -> IO ()-}
{-setWidthFromDuration widget duration =-}
  {-let width = fromIntegral (ceiling duration :: Int) * 50-}
  {-in  Gtk.widgetSetSizeRequest widget width (-1)-}

focusedClass :: Focused -> Text
focusedClass = \case
  Focused             -> "focused"
  TransitivelyFocused -> "transitively-focused"
  Blurred             -> "blurred"

renderClip' :: Focused -> ClipMetadata -> Element
renderClip' focused metadata =
  {-Gtk.labelSetEllipsize label EllipsizeModeEnd-}
  {-setWidthFromDuration  label (duration metadata)-}
  Box Horizontal ["clip", focusedClass focused] [Label (clipName metadata) []]

renderGap :: Focused -> NominalDiffTime -> Element
renderGap focused _duration =
  {-Gtk.labelSetEllipsize label EllipsizeModeEnd-}
  {-setWidthFromDuration  label (duration metadata)-}
  Box Horizontal ["gap", focusedClass focused] [Label "" []]

renderClip :: Clip Focused t -> Element
renderClip = \case
  VideoClip focused metadata -> renderClip' focused metadata
  AudioClip focused metadata -> renderClip' focused metadata
  VideoGap  focused duration -> renderGap focused duration
  AudioGap  focused duration -> renderGap focused duration

renderSequence :: Sequence Focused -> Element
renderSequence = \case
  Sequence focused sub ->
    Box Horizontal ["sequence", focusedClass focused] (map renderSequence sub)
  Composition focused videoClips audioClips -> Box
    Vertical
    ["composition", focusedClass focused]
    [ Box Horizontal ["video"] (map renderClip videoClips)
    , Box Horizontal ["audio"] (map renderClip audioClips)
    ]

renderScene :: Scene -> Element
renderScene Scene {..} =
  Box Vertical ["scene"] [Label sceneName mempty, renderSequence (applyFocus topSequence focus)]
