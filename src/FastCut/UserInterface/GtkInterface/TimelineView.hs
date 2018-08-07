{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The main view of FastCut's GTK interface.
module FastCut.UserInterface.GtkInterface.TimelineView
  ( timelineView
  ) where

import           FastCut.Prelude                         hiding (on)

import           Control.Lens
import           Data.Int                                (Int32)
import           Data.Text                               (Text)
import           GI.Gtk.Declarative                      as Gtk

import           FastCut.Composition
import           FastCut.Composition.Focused
import           FastCut.Duration
import           FastCut.Focus
import           FastCut.Library
import           FastCut.Project
import           FastCut.UserInterface
import           FastCut.UserInterface.GtkInterface.View

widthFromDuration :: (RealFrac d) => d -> Int32
widthFromDuration duration' = fromIntegral (ceiling duration' :: Int) * 10

focusedClass :: Focused -> Text
focusedClass = \case
  Focused             -> "focused"
  TransitivelyFocused -> "transitively-focused"
  Blurred             -> "blurred"

renderClipAsset :: Focused -> Asset mt -> Markup
renderClipAsset focused asset' = container
  Box
  [ classes ["clip", focusedClass focused]
  , #orientation := OrientationHorizontal
  , #widthRequest := widthFromDuration (durationOf asset')
  , #tooltipText := toS (asset' ^. assetMetadata . path)
  ]
  [BoxChild False False 0 $ node Label []]

renderPart :: CompositionPart Focused t -> Markup
renderPart = \case
  Clip focused asset'    -> renderClipAsset focused asset'
  Gap  focused duration' -> container
    Box
    [ classes ["gap", focusedClass focused]
    , #orientation := OrientationHorizontal
    , #widthRequest := widthFromDuration duration'
    ]
    [node Label []]

renderComposition :: Composition Focused t -> Markup
renderComposition = \case
  Timeline _ sub -> container
    Box
    [classes ["composition", "timeline", emptyClass (null sub)]]
    (map renderComposition (toList sub))
  Sequence focused sub -> container
    Box
    [ classes
        ["composition", "sequence", focusedClass focused, emptyClass (null sub)]
    ]
    (map renderComposition (toList sub))
  Parallel focused vs as -> container
    Box
    [ #orientation := OrientationVertical
    , classes
      [ "composition"
      , "parallel"
      , focusedClass focused
      , emptyClass (null vs && null as)
      ]
    ]
    [ BoxChild False False 0
      $ container
          Box
          [classes ["video", focusedClass focused]]
          (map renderPart vs)
    , BoxChild False False 0
      $ container
          Box
          [classes ["audio", focusedClass focused]]
          (map renderPart as)
    ]
 where
  emptyClass True  = "empty"
  emptyClass False = "non-empty"

renderPreviewPane :: Maybe (FirstCompositionPart a) -> Markup
renderPreviewPane = \case
  Just (FirstVideoPart (Clip _ (VideoAsset meta))) ->
    node Image [#file := toS (meta ^. thumbnail)]
  Just (FirstAudioPart (Clip _ (AudioAsset _meta))) ->
    node Label [#label := "Audio clip..."]
  Just (FirstVideoPart Gap{}) -> node Label [#label := "Video gap."]
  Just (FirstAudioPart Gap{}) -> node Label [#label := "Audio gap."]
  Nothing                     -> node Label [#label := "No preview available."]

timelineView :: Project -> Focus ft -> IO (View TimelineMode)
timelineView project focus = viewWithEvents $ \_ -> container
  Box
  [#orientation := OrientationVertical]
  [ BoxChild
    True
    True
    0
    (renderPreviewPane (firstCompositionPart focus (project ^. timeline)))
  , BoxChild False False 0 $ container
    ScrolledWindow
    [ #hscrollbarPolicy := PolicyTypeAutomatic
    , #vscrollbarPolicy := PolicyTypeNever
    , classes ["timeline-container"]
    ]
    (renderComposition (applyFocus (project ^. timeline) focus))
  ]
