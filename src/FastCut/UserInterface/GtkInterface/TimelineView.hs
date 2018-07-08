{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The main view of FastCut's GTK interface.
module FastCut.UserInterface.GtkInterface.TimelineView
  ( timelineView
  ) where

import           FastCut.Prelude

import           Control.Lens
import           Data.Int                    (Int32)
import           Data.Text                   (Text)

import           FastCut.Composition
import           FastCut.Composition.Focused
import           FastCut.Focus
import           FastCut.Project

import           GI.Gtk.Declarative          as Gtk

widthFromDuration :: (RealFrac d) => d -> Int32
widthFromDuration duration' = fromIntegral (ceiling duration' :: Int) * 50

focusedClass :: Focused -> Text
focusedClass = \case
  Focused             -> "focused"
  TransitivelyFocused -> "transitively-focused"
  Blurred             -> "blurred"

renderClip' :: Focused -> ClipMetadata -> Markup
renderClip' focused metadata =
  container
    Box
    [ classes ["clip", focusedClass focused]
    , #orientation := OrientationHorizontal
    , #widthRequest := widthFromDuration (duration metadata)
    ]
    [ BoxChild False False 0 $
      node Label [#label := clipName metadata]
    ]

renderPart :: CompositionPart Focused t -> Markup
renderPart =
  \case
    Clip (VideoClip focused metadata) -> renderClip' focused metadata
    Clip (AudioClip focused metadata) -> renderClip' focused metadata
    Gap focused duration' ->
      container
        Box
        [ classes ["gap", focusedClass focused]
        , #orientation := OrientationHorizontal
        , #widthRequest := widthFromDuration duration'
        ]
        [node Label []]

renderComposition :: Composition Focused t -> Markup
renderComposition =
  \case
    Timeline _ sub ->
      container
        Box
        [ classes ["composition", "timeline"]]
        (map renderComposition sub)
    Sequence focused sub ->
      container
        Box
        [ classes ["composition", "sequence", focusedClass focused]
        , #widthRequest := if null sub then 10 else 0
        ]
        (map renderComposition sub)
    Parallel focused vs as ->
      container
        Box
        [ #orientation := OrientationVertical
        , classes ["composition", "parallel", focusedClass focused]
        ]
        [ BoxChild False False 0 $
          container
            Box
            [classes ["video", focusedClass focused]]
            (map renderPart vs)
        , BoxChild False False 0 $
          container
            Box
            [classes ["audio", focusedClass focused]]
            (map renderPart as)
        ]

timelineView :: Project -> Focus ft -> Markup
timelineView project focus =
  container
    Box
    [#orientation := OrientationVertical, classes ["timeline-container"]]
    [ BoxChild True True 0 $ node Label [#label := (project ^. projectName)]
    , BoxChild False False 0 $
      container
        ScrolledWindow
        [ #hscrollbarPolicy := PolicyTypeAutomatic
        , #vscrollbarPolicy := PolicyTypeNever
        ]
        (renderComposition
           (applyFocus
              (project ^. timeline)
              focus))
    ]
