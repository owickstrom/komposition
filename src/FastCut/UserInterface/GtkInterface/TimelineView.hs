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
import           GI.Gtk                      (Align (..), Box (..), Button (..),
                                              Image (..), Label (..),
                                              MenuBar (..), MenuItem (..),
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

renderClipAsset ::
     Focus SequenceFocusType
  -> Focused
  -> Asset mt
  -> Widget (Event TimelineMode)
renderClipAsset thisFocus focused asset' =
  container
    Box
    [ classes ["clip", focusedClass focused]
    , #orientation := OrientationHorizontal
    , #tooltipText := toS (asset' ^. assetMetadata . path)
    ] $
  boxChild False False 0 $
  widget Button [on #clicked (CommandKeyMappedEvent (JumpFocus thisFocus))
                , #widthRequest := widthFromDuration (durationOf asset')
                ]

renderPart :: CompositionPart t (Focus SequenceFocusType, Focused) -> Widget (Event TimelineMode)
renderPart =
  \case
    Clip (thisFocus, focused) asset' -> renderClipAsset thisFocus focused asset'
    Gap (thisFocus, focused) duration' ->
      container
        Box
        [ classes ["gap", focusedClass focused]
        , #orientation := OrientationHorizontal
        ] $
      boxChild
        False
        False
        0
        (widget
           Button
           [on #clicked (CommandKeyMappedEvent (JumpFocus thisFocus))
           , #widthRequest := widthFromDuration duration'
           ])

renderTimeline :: Timeline (Focus SequenceFocusType, Focused) -> Widget (Event TimelineMode)
renderTimeline (Timeline sub) =
  container
    Box
    [classes ["composition", "timeline", emptyClass (null sub)]]
    (mapM_ (boxChild False False 0 . renderSequence) (toList sub))

renderSequence :: Sequence (Focus SequenceFocusType, Focused) -> Widget (Event TimelineMode)
renderSequence (Sequence (_thisFocus, focused) sub) =
  container
    Box
    [ classes
        ["composition", "sequence", focusedClass focused, emptyClass (null sub)]
    ]
    (mapM_ (boxChild False False 0 . renderParallel) (toList sub))

renderParallel :: Parallel (Focus SequenceFocusType, Focused) -> Widget (Event TimelineMode)
renderParallel (Parallel (_thisFocus, focused) vs as) =
  container
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

emptyClass :: Bool -> Text
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

renderMenu :: Widget (Event TimelineMode)
renderMenu =
  container MenuBar [] $ do
    subMenu "Project" $ do
      labelledItem Import
      labelledItem Render
      labelledItem Exit
    subMenu "Timeline" $ do
      subMenu "Insert Clip" $
        forM_ (enumFrom minBound) (labelledItem . InsertCommand InsertClip)
      subMenu "Insert Gap" $
        forM_ (enumFrom minBound) (labelledItem . InsertCommand InsertGap)
      labelledItem Delete
    subMenu "Help" $ do
      labelledItem Help
  where
    labelledItem cmd =
      menuItem MenuItem [on #activate (CommandKeyMappedEvent cmd)] $
      widget Label [#label := commandName cmd, #halign := AlignStart]

timelineView :: Project -> Focus SequenceFocusType -> Widget (Event TimelineMode)
timelineView project focus =
  container Box [#orientation := OrientationVertical] $ do
    boxChild False False 0 renderMenu
    boxChild
      True
      True
      0
      (renderPreviewPane (firstCompositionPart focus (project ^. timeline)))
    boxChild False False 0 $
      bin
        ScrolledWindow
        [ #hscrollbarPolicy := PolicyTypeAutomatic
        , #vscrollbarPolicy := PolicyTypeNever
        , classes ["timeline-container"]
        ]
        (renderTimeline focusedTimelineWithSetFoci)
  where
    focusedTimelineWithSetFoci :: Timeline (Focus SequenceFocusType, Focused)
    focusedTimelineWithSetFoci =
      withAllFoci (project ^. timeline)
      <&> \f -> (f, focusedState focus f)
