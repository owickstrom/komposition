{-# LANGUAGE FlexibleContexts #-}
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
                                              Scale (..),
                                              ScrolledWindow (..))
import           GI.Gtk.Declarative

import           FastCut.Composition
import           FastCut.MediaType
import           FastCut.Composition.Focused
import           FastCut.Duration
import           FastCut.Focus
import           FastCut.Library
import           FastCut.Project
import           FastCut.UserInterface

widthFromDuration :: ZoomLevel -> Duration -> Int32
widthFromDuration (ZoomLevel zl) duration' =
  round (durationToSeconds duration' * 5 * zl)

focusedClass :: Focused -> Text
focusedClass = \case
  Focused             -> "focused"
  TransitivelyFocused -> "transitively-focused"
  Blurred             -> "blurred"

renderClipAsset ::
     AssetMetadataLens asset
  => HasDuration asset
  => ZoomLevel
  -> Focus SequenceFocusType
  -> Focused
  -> asset
  -> Duration
  -> Widget (Event TimelineMode)
renderClipAsset zl thisFocus focused asset' duration' =
  container
    Box
    [ classes ["clip", focusedClass focused]
    , #orientation := OrientationHorizontal
    , #tooltipText := toS (asset' ^. assetMetadata . path . unOriginalPath)
    ] $
  boxChild False False 0 $
  widget Button [on #clicked (CommandKeyMappedEvent (JumpFocus thisFocus))
                , #widthRequest := widthFromDuration zl duration'
                , #hasFocus := (focused == Focused)
                ]

renderGap ::
     ZoomLevel
  -> (Focus SequenceFocusType, Focused)
  -> Duration
  -> Widget (Event TimelineMode)
renderGap zl (thisFocus, focused) duration' =
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
           , #widthRequest := widthFromDuration zl duration'
           , #hasFocus := (focused == Focused)
           ])

renderVideoPart ::
     ZoomLevel
  -> VideoPart (Focus SequenceFocusType, Focused)
  -> Widget (Event TimelineMode)
renderVideoPart zl =
  \case
    VideoClip (thisFocus, focused) asset' ts _ -> renderClipAsset zl thisFocus focused asset' (durationOf ts)
    VideoGap ann duration' -> renderGap zl ann duration'

renderAudioPart ::
     ZoomLevel
  -> AudioPart (Focus SequenceFocusType, Focused)
  -> Widget (Event TimelineMode)
renderAudioPart zl =
  \case
    AudioClip (thisFocus, focused) asset' -> renderClipAsset zl thisFocus focused asset' (durationOf asset')
    AudioGap ann duration' -> renderGap zl ann duration'

renderTimeline :: ZoomLevel -> Timeline (Focus SequenceFocusType, Focused) -> Widget (Event TimelineMode)
renderTimeline zl (Timeline sub) =
  container
    Box
    [classes ["composition", "timeline", emptyClass (null sub)]]
    (mapM_ (boxChild False False 0 . renderSequence zl) (toList sub))

renderSequence :: ZoomLevel -> Sequence (Focus SequenceFocusType, Focused) -> Widget (Event TimelineMode)
renderSequence zl (Sequence (_thisFocus, focused) sub) =
  container
    Box
    [ classes
        ["composition", "sequence", focusedClass focused, emptyClass (null sub)]
    ]
    (mapM_ (boxChild False False 0 . renderParallel zl) (toList sub))

renderParallel :: ZoomLevel -> Parallel (Focus SequenceFocusType, Focused) -> Widget (Event TimelineMode)
renderParallel zl (Parallel (_thisFocus, focused) vs as) =
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
          (mapM_ (boxChild False False 0 . renderVideoPart zl) vs)
    boxChild False False 0
      $ container
          Box
          [classes ["audio", focusedClass focused]]
          (mapM_ (boxChild False False 0 . renderAudioPart zl) as)

emptyClass :: Bool -> Text
emptyClass True  = "empty"
emptyClass False = "non-empty"

renderPreviewPane :: Maybe (FirstCompositionPart a) -> Widget (Event TimelineMode)
renderPreviewPane part =
  container Box [classes ["preview-pane"]] $ boxChild True True 0 $
    case part of
      Just (FirstVideoPart (VideoClip _ _videoAsset _ thumbnail)) ->
        thumbnailImage (toS thumbnail)
      Just (FirstAudioPart AudioClip{}) ->
        noPreviewAvailable
      Just (FirstVideoPart VideoGap{}) -> widget Label [#label := "Video gap."]
      Just (FirstAudioPart AudioGap{}) -> widget Label [#label := "Audio gap."]
      Nothing                     -> noPreviewAvailable
  where
    thumbnailImage thumbnailFile =
       widget Image [#file := thumbnailFile]
    noPreviewAvailable = widget Label [#label := "No preview available."]

renderMenu :: Widget (Event TimelineMode)
renderMenu =
  container MenuBar [] $ do
    subMenu "Project" $ do
      labelledItem Import
      labelledItem Render
      labelledItem Exit
    subMenu "Timeline" $ do
      insertSubMenu Video
      insertSubMenu Audio
      labelledItem Split
      labelledItem Delete
    subMenu "Help" $ do labelledItem Help
  where
    labelledItem cmd =
      menuItem MenuItem [on #activate (CommandKeyMappedEvent cmd)] $
      widget Label [#label := commandName cmd, #halign := AlignStart]
    insertSubMenu mediaType' =
      subMenu ("Insert " <> show mediaType') $ do
        subMenu "Clip" $
          forM_
            (enumFrom minBound)
            (labelledItem . InsertCommand (InsertClip (Just mediaType')))
        subMenu " Gap" $
          forM_
            (enumFrom minBound)
            (labelledItem . InsertCommand (InsertGap (Just mediaType')))

timelineView :: TimelineModel -> Widget (Event TimelineMode)
timelineView model =
  container Box [#orientation := OrientationVertical] $ do
    boxChild False False 0 renderMenu
    boxChild
      True
      True
      0
      (renderPreviewPane
         (firstCompositionPart (model ^. currentFocus) (model ^. project . timeline)))
    boxChild False False 0 $
      bin
        ScrolledWindow
        [ #hscrollbarPolicy := PolicyTypeAutomatic
        , #vscrollbarPolicy := PolicyTypeNever
        , classes ["timeline-container"]
        ]
        (renderTimeline (model ^. zoomLevel) focusedTimelineWithSetFoci)
    boxChild False False 0 $
      widget
        Scale
        [ classes ["zoom-level"]
        , onM #valueChanged onZoomLevelChange
        , afterCreated afterZoomLevelCreate
        , #drawValue := False
        ]
  where
    focusedTimelineWithSetFoci :: Timeline (Focus SequenceFocusType, Focused)
    focusedTimelineWithSetFoci =
      withAllFoci (model ^. project . timeline) <&> \f ->
        (f, focusedState (model ^. currentFocus) f)

    afterZoomLevelCreate :: Scale -> IO ()
    afterZoomLevelCreate scale =
      #setRange scale 1 9

    onZoomLevelChange = fmap (ZoomLevelChanged . ZoomLevel) . #getValue
