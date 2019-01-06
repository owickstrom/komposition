{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The main view of Komposition's GTK interface.
module Komposition.UserInterface.GtkInterface.TimelineView
  ( timelineView
  )
where

import           Komposition.Prelude                                     hiding
                                                                          (on)

import           Control.Lens
import           Data.Int                                                (Int32)
import           Data.Text                                               (Text)
import qualified Data.Vector                                             as Vector
import           GI.Gtk                                                  (Align (..),
                                                                          Box (..),
                                                                          Button (..),
                                                                          Label (..),
                                                                          MenuBar (..),
                                                                          MenuItem (..),
                                                                          Orientation (..),
                                                                          PolicyType (..),
                                                                          ScrolledWindow (..),
                                                                          Window (..))
import           GI.Gtk.Declarative
import           GI.Pango                                                (EllipsizeMode (..))

import           Komposition.Composition
import           Komposition.Composition.Focused
import           Komposition.Composition.Paste                           (PastePosition (..))
import           Komposition.Duration
import           Komposition.Focus
import           Komposition.Library
import           Komposition.MediaType
import           Komposition.Project
import           Komposition.Timestamp
import           Komposition.UserInterface                               hiding (Window,
                                                                          timelineView)
import           Komposition.UserInterface.GtkInterface.NumberInput      as NumberInput
import           Komposition.UserInterface.GtkInterface.RangeSlider
import           Komposition.UserInterface.GtkInterface.ThumbnailPreview
import           Komposition.VideoSettings
import           Komposition.VideoSpeed

widthFromDuration :: ZoomLevel -> Duration -> Int32
widthFromDuration (ZoomLevel zl) duration' =
  round (durationToSeconds duration' * 5 * zl)

focusedClass :: Focused -> Text
focusedClass = \case
  Focused             -> "focused"
  TransitivelyFocused -> "transitively-focused"
  Blurred             -> "blurred"

renderClipAsset
  :: AssetMetadataLens asset
  => HasDuration asset
  => ZoomLevel
  -> Focus SequenceFocusType
  -> Focused
  -> asset
  -> Duration
  -> Widget (Event TimelineMode)
renderClipAsset zl thisFocus focused asset' duration' = container
  Box
  [ classes ["clip", focusedClass focused]
  , #orientation := OrientationHorizontal
  , #tooltipText := toS (asset' ^. assetMetadata . path . unOriginalPath)
  ]
  [ BoxChild defaultBoxChildProperties { expand = False, fill = False, padding = 0 } $ widget
      Button
      [ on #clicked (CommandKeyMappedEvent (JumpFocus thisFocus))
      , #widthRequest := widthFromDuration zl duration'
      , #hasFocus := (focused == Focused)
      ]
  ]

renderGap
  :: ZoomLevel
  -> (Focus SequenceFocusType, Focused)
  -> Duration
  -> Widget (Event TimelineMode)
renderGap zl (thisFocus, focused) duration' = container
  Box
  [classes ["gap", focusedClass focused], #orientation := OrientationHorizontal]
  [
      widget
        Button
        [ on #clicked (CommandKeyMappedEvent (JumpFocus thisFocus))
        , #widthRequest := widthFromDuration zl duration'
        , #hasFocus := (focused == Focused)
        ]
  ]

renderVideoPart
  :: ZoomLevel
  -> VideoPart (Focus SequenceFocusType, Focused)
  -> Widget (Event TimelineMode)
renderVideoPart zl = \case
  c@(VideoClip (thisFocus, focused) asset' _ _ _) ->
    renderClipAsset zl thisFocus focused asset' (durationOf AdjustedDuration c)
  VideoGap ann duration' -> renderGap zl ann duration'

renderAudioPart
  :: ZoomLevel
  -> AudioPart (Focus SequenceFocusType, Focused)
  -> Widget (Event TimelineMode)
renderAudioPart zl = \case
  AudioClip (thisFocus, focused) asset' ->
    renderClipAsset zl thisFocus focused asset' (durationOf AdjustedDuration asset')
  AudioGap ann duration' -> renderGap zl ann duration'

renderTimeline
  :: ZoomLevel
  -> Timeline (Focus SequenceFocusType, Focused)
  -> Widget (Event TimelineMode)
renderTimeline zl (Timeline sub) = container
  Box
  [classes ["composition", "timeline", emptyClass (null sub)]]
  (map (BoxChild defaultBoxChildProperties { expand = False, fill = False, padding = 0 } . renderSequence zl) (Vector.fromList $ toList sub))

renderSequence
  :: ZoomLevel
  -> Sequence (Focus SequenceFocusType, Focused)
  -> Widget (Event TimelineMode)
renderSequence zl (Sequence (_thisFocus, focused) sub) = container
  Box
  [ classes
      ["composition", "sequence", focusedClass focused, emptyClass (null sub)]
  ]
  (map (renderParallel zl) (Vector.fromList $ toList sub))

renderParallel
  :: ZoomLevel
  -> Parallel (Focus SequenceFocusType, Focused)
  -> BoxChild (Event TimelineMode)
renderParallel zl (Parallel (_thisFocus, focused) vs as) = container
  Box
  [ #orientation := OrientationVertical
  , classes
    [ "composition"
    , "parallel"
    , focusedClass focused
    , emptyClass (null vs && null as)
    ]
  ]
  [ container
    Box
    [classes ["video", focusedClass focused]]
    (fmap
      ( BoxChild defaultBoxChildProperties { expand  = False
                                           , fill    = False
                                           , padding = 0
                                           }
      . renderVideoPart zl
      )
      (Vector.fromList vs)
    )
  , container
    Box
    [classes ["audio", focusedClass focused]]
    (fmap
      ( BoxChild defaultBoxChildProperties { expand  = False
                                           , fill    = False
                                           , padding = 0
                                           }
      . renderAudioPart zl
      )
      (Vector.fromList as)
    )
  ]

emptyClass :: Bool -> Text
emptyClass True  = "empty"
emptyClass False = "non-empty"

renderPreviewPane
  :: Maybe FilePath -> Pane (Event TimelineMode)
renderPreviewPane path' = pane defaultPaneProperties $ container
  Box
  [classes ["preview-pane"]]
  [ BoxChild defaultBoxChildProperties { expand = True, fill = True, padding = 0 } $ case path' of
      Just p  -> thumbnailPreview p
      Nothing -> noPreviewAvailable
  ]
  where noPreviewAvailable = widget Label [#label := "No preview available."]

durationEntry :: VideoSettings -> (Duration, Duration) -> Duration -> Widget Duration
durationEntry vs range' current = toDuration <$> numberInput NumberInputProperties
  { value              = durationToSeconds current
  , NumberInput.range  = range' & both %~ durationToSeconds
  , step               = 1 / fromIntegral (vs ^. frameRate)
  , digits             = 2
  , numberInputClasses = []
  }
  where toDuration (NumberInputChanged n) = durationFromSeconds n

clipSpanControl :: VideoSettings -> VideoAsset -> TimeSpan -> BoxChild (Event TimelineMode)
clipSpanControl vs asset ts = container
  Box
  [#orientation := OrientationHorizontal]
  [ BoxChild defaultBoxChildProperties { expand  = True
                                       , fill    = True
                                       , padding = 5
                                       }
  $   FocusedClipStartSet
  <$> durationEntry vs (0, spanEnd ts) (spanStart ts)
  , BoxChild defaultBoxChildProperties { expand  = True
                                       , fill    = True
                                       , padding = 5
                                       }
  $   FocusedClipEndSet
  <$> durationEntry vs (spanStart ts, asset ^. assetMetadata . duration) (spanEnd ts)
  ]

renderSidebar
  :: VideoSettings -> Maybe (SomeComposition a) -> Pane (Event TimelineMode)
renderSidebar vs mcomp = pane defaultPaneProperties $ container
  Box
  [ #orientation := OrientationVertical
  , #widthRequest := 40
  , classes ["sidebar"]
  ]
  inner
  where
    inner = case mcomp of
      Just (SomeSequence s) ->
        [ heading "Sequence"
        , entry "Duration" (formatDuration (durationOf AdjustedDuration s))
        ]
      Just (SomeParallel p) ->
        [ heading "Parallel"
        , entry "Duration" (formatDuration (durationOf AdjustedDuration p))
        ]
      Just (SomeVideoPart (VideoClip _ asset ts speed _))
        -> [ heading "Video Clip"
           , entry "Duration" (formatDuration (durationOf AdjustedDuration ts))
           , entry "Speed"    (formatSpeed speed)
           , heading "Start/End"
           , clipSpanControl vs asset ts
           , heading "Video Asset"
           , entry "Original"
                   (toS (asset ^. videoAssetMetadata . path . unOriginalPath))
           , entry "Duration"
                   (formatDuration (asset ^. videoAssetMetadata . duration))
           ]
      Just (SomeVideoPart (VideoGap _ d)) ->
        [heading "Video Gap", entry "Duration" (formatDuration d)]
      Just (SomeAudioPart (AudioClip _ asset))
        -> [ heading "Audio Clip"
           , entry "Duration"
                   (formatDuration (asset ^. audioAssetMetadata . duration))
           , heading "Audio Asset"
           , entry
             "Original"
             (show (asset ^. audioAssetMetadata . path . unOriginalPath))
           ]
      Just (SomeAudioPart (AudioGap _ d)) ->
        [heading "Audio Gap", entry "Duration" (formatDuration d)]
      Nothing ->
        [ BoxChild defaultBoxChildProperties { expand  = False
                                             , fill    = False
                                             , padding = 0
                                             }
            $ widget Label [#label := "Nothing focused."]
        ]
    heading :: Text -> BoxChild (Event TimelineMode)
    heading t =
      BoxChild defaultBoxChildProperties { expand  = False
                                         , fill    = False
                                         , padding = 0
                                         }
        $ widget Label [#label := t, classes ["sidebar-heading"]]
    entry :: Text -> Text -> BoxChild (Event TimelineMode)
    entry name val
      = BoxChild defaultBoxChildProperties { expand  = False
                                           , fill    = False
                                           , padding = 0
                                           }
        $ container
            Box
            [#orientation := OrientationHorizontal, classes ["sidebar-entry"]]
            [ BoxChild defaultBoxChildProperties { expand  = True
                                                 , fill    = True
                                                 , padding = 0
                                                 }
              $ widget Label [#label := name, #halign := AlignStart]
            , BoxChild defaultBoxChildProperties { expand  = False
                                                 , fill    = False
                                                 , padding = 0
                                                 }
              $ widget Label [#label := val, #ellipsize := EllipsizeModeEnd]
            ]
    formatDuration = printTimestampWithPrecision (Just 2)

renderMainArea
  :: TimelineModel -> Widget (Event TimelineMode)
renderMainArea model =
  paned [#orientation := OrientationHorizontal, #wideHandle := True]
  (renderPreviewPane (model ^. previewImagePath))
  (renderSidebar (project' ^. videoSettings . renderVideoSettings) (atFocus currentFocus' (project' ^. timeline)))
  where
    project' = currentProject model
    currentFocus' = model ^. currentFocus


renderMenu :: Widget (Event TimelineMode)
renderMenu = container
  MenuBar
  []
  [ subMenu
    "Project"
    [ labelledItem SaveProject
    , labelledItem CloseProject
    , labelledItem Import
    , labelledItem Render
    , labelledItem Exit
    ]
  , subMenu
    "Timeline"
    [ labelledItem Copy
    , subMenu
      "Paste"
      [labelledItem (Paste PasteRightOf), labelledItem (Paste PasteLeftOf)]
    , insertSubMenu Video
    , insertSubMenu Audio
    , labelledItem Split
    , labelledItem Delete
    ]
  , subMenu "Help" [labelledItem Help]
  ]
  where
    labelledItem cmd =
      menuItem MenuItem [on #activate (CommandKeyMappedEvent cmd)]
        $ widget Label [#label := commandName cmd, #halign := AlignStart]
    insertSubMenu mediaType' = subMenu
      ("Insert " <> show mediaType')
      [ subMenu
        "Clip"
        (   Vector.enumFromTo minBound maxBound
        <&> (labelledItem . InsertCommand (InsertClip (Just mediaType')))
        )
      , subMenu " Gap"
        (   Vector.enumFromTo minBound maxBound
        <&> (labelledItem . InsertCommand (InsertGap (Just mediaType')))
        )
      ]

renderBottomBar :: TimelineModel -> Widget (Event TimelineMode)
renderBottomBar model = container
  Box
  [#orientation := OrientationHorizontal, classes ["bottom-bar"]]
  [ BoxChild defaultBoxChildProperties { expand = True, fill = True, padding = 0 } $ widget
    Label
    [ classes ["status-message"]
    , #label := fromMaybe "" (model ^. statusMessage)
    , #ellipsize := EllipsizeModeEnd
    , #halign := AlignStart
    ]
  , BoxChild defaultBoxChildProperties { expand = False, fill = False, padding = 0 } $ toZoomEvent <$> rangeSlider
    (RangeSliderProperties (1, 9) ["zoom-level"])
  ]
  where toZoomEvent (RangeSliderChanged d) = ZoomLevelChanged (ZoomLevel d)

timelineView :: TimelineModel -> Bin Window (Event TimelineMode)
timelineView model =
  bin
      Window
      [ #title := (currentProject model ^. projectName)
      , on #deleteEvent (const (True, WindowClosed))
      ]
    $ container
        Box
        [#orientation := OrientationVertical]
        [ BoxChild defaultBoxChildProperties renderMenu
        , BoxChild defaultBoxChildProperties { expand = True, fill = True, padding = 0 }
          (renderMainArea model
          )
        , bin
          ScrolledWindow
          [ #hscrollbarPolicy := PolicyTypeAutomatic
          , #vscrollbarPolicy := PolicyTypeNever
          , classes ["timeline-container"]
          ]
          (renderTimeline (model ^. zoomLevel) focusedTimelineWithSetFoci)
        , BoxChild defaultBoxChildProperties $ renderBottomBar model
        ]
  where
    focusedTimelineWithSetFoci :: Timeline (Focus SequenceFocusType, Focused)
    focusedTimelineWithSetFoci = withAllFoci (currentProject model ^. timeline)
      <&> \f -> (f, focusedState (model ^. currentFocus) f)
