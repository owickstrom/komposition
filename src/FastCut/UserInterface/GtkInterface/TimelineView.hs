{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The main view of FastCut's GTK interface.
module FastCut.UserInterface.GtkInterface.TimelineView
  ( timelineView
  ) where

import           FastCut.Prelude             hiding (on)

import           Control.Lens
import           Data.Int                    (Int32)
import           Data.Text                   (Text)
import           GI.Gtk                      (Box (..), Image (..), Label (..),
                                              Orientation (..), PolicyType (..),
                                              ScrolledWindow (..))
import           GI.Gtk.Declarative

import           FastCut.Composition
import           FastCut.Composition.Focused
import           FastCut.Duration
import           FastCut.Focus
import           FastCut.Library
import           FastCut.Project
import           FastCut.UserInterface

widthFromDuration :: Duration -> Int32
widthFromDuration duration' =
  round (durationToSeconds duration' * 50)

focusedClass :: Focused -> Text
focusedClass = \case
  Focused             -> "focused"
  TransitivelyFocused -> "transitively-focused"
  Blurred             -> "blurred"

renderClipAsset :: Focused -> Asset mt -> Widget (Event TimelineMode)
renderClipAsset focused asset' = container
  Box
  [ classes ["clip", focusedClass focused]
  , #orientation := OrientationHorizontal
  , #widthRequest := widthFromDuration (durationOf asset')
  , #tooltipText := toS (asset' ^. assetMetadata . path)
  ] $ boxChild False False 0 $ widget Label []

renderPart :: CompositionPart Focused t -> Widget (Event TimelineMode)
renderPart = \case
  Clip focused asset'    -> renderClipAsset focused asset'
  Gap  focused duration' -> container
    Box
    [ classes ["gap", focusedClass focused]
    , #orientation := OrientationHorizontal
    , #widthRequest := widthFromDuration duration'
    ] $ boxChild False False 0 (widget Label [])

renderComposition :: Composition Focused t -> Widget (Event TimelineMode)
renderComposition = \case
  Timeline _ sub -> container
    Box
    [classes ["composition", "timeline", emptyClass (null sub)]]
    (mapM_ (boxChild False False 0 . renderComposition) (toList sub))
  Sequence focused sub -> container
    Box
    [ classes
        ["composition", "sequence", focusedClass focused, emptyClass (null sub)]
    ]
    (mapM_ (boxChild False False 0 . renderComposition) (toList sub))
  Parallel focused vs as -> container
    Box
    [ #orientation := OrientationVertical
    , classes
      [ "composition"
      , "parallel"
      , focusedClass focused
      , emptyClass (null vs && null as)
      ]
    ] $ do
    boxChild False False 0
      $ container
          Box
          [classes ["video", focusedClass focused]]
          (mapM_ (boxChild False False 0 . renderPart) vs)
    boxChild False False 0
      $ container
          Box
          [classes ["audio", focusedClass focused]]
          (mapM_ (boxChild False False 0 . renderPart) as)
 where
  emptyClass True  = "empty"
  emptyClass False = "non-empty"

renderPreviewPane :: Maybe (FirstCompositionPart a) -> Widget (Event TimelineMode)
renderPreviewPane = \case
  Just (FirstVideoPart (Clip _ (VideoAsset meta))) ->
    thumbnailImageOrPlaceholder (meta ^. thumbnail)
  Just (FirstAudioPart (Clip _ (AudioAsset meta))) ->
    thumbnailImageOrPlaceholder (meta ^. thumbnail)
  Just (FirstVideoPart Gap{}) -> widget Label [#label := "Video gap."]
  Just (FirstAudioPart Gap{}) -> widget Label [#label := "Audio gap."]
  Nothing                     -> noPreviewAvailable

  where
    thumbnailImageOrPlaceholder = \case
      Just thumbnailFile -> widget Image [#file := toS thumbnailFile]
      Nothing -> noPreviewAvailable
    noPreviewAvailable = widget Label [#label := "No preview available."]

timelineView :: Project -> Focus ft -> Widget (Event TimelineMode)
timelineView project focus = container
  Box
  [#orientation := OrientationVertical] $ do
  boxChild
    True
    True
    0
    (renderPreviewPane (firstCompositionPart focus (project ^. timeline)))
  boxChild False False 0 $ bin
    ScrolledWindow
    [ #hscrollbarPolicy := PolicyTypeAutomatic
    , #vscrollbarPolicy := PolicyTypeNever
    , classes ["timeline-container"]
    ]
    (renderComposition (applyFocus (project ^. timeline) focus))
