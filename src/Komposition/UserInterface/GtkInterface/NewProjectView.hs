{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Komposition.UserInterface.GtkInterface.NewProjectView
  ( newProjectView
  )
where

import           Control.Lens
import           Komposition.Prelude                                hiding
                                                                     (State, on)

import           GI.Gtk                                             (Align (..),
                                                                     Box (..),
                                                                     Button (..),
                                                                     Dialog (..),
                                                                     Dialog (..),
                                                                     Entry (..),
                                                                     Label (..),
                                                                     Orientation (..),
                                                                     entryGetText)
import           GI.Gtk.Declarative                                 as Gtk

import           Komposition.UserInterface                          hiding (newProjectView)
import           Komposition.UserInterface.GtkInterface.NumberInput
import           Komposition.UserInterface.GtkInterface.SelectBox
import           Komposition.VideoSettings

newProjectView :: NewProjectModel -> Bin Dialog (Event 'NewProjectMode)
newProjectView model =
  bin Dialog
      [#title := "New Project", on #deleteEvent (const (True, WindowClosed))]
    $ container
        Box
        [classes ["new-project-view"], #orientation := OrientationVertical]
        [ BoxChild defaultBoxChildProperties {padding =  10} $ container
          Box
          [#orientation := OrientationHorizontal]
          [BoxChild defaultBoxChildProperties { expand = True, fill = True } $ nameControl model]
        , BoxChild defaultBoxChildProperties { expand = True, fill = True, padding = 10 } $ container
          Box
          [#orientation := OrientationHorizontal, #valign := AlignStart]
          [ BoxChild defaultBoxChildProperties { expand = True, fill = True, padding = 10 } $ frameRateControl model
          , BoxChild defaultBoxChildProperties { expand = True, fill = True, padding = 10 } $ resolutionControl model
          ]
        , BoxChild defaultBoxChildProperties { padding = 10 } $ container
          Box
          [#orientation := OrientationHorizontal, #halign := AlignEnd]
          [ BoxChild defaultBoxChildProperties { expand = True, fill = False, padding = 5 }
            $ widget Button [#label := "Cancel", on #clicked WindowClosed]
          , BoxChild defaultBoxChildProperties { expand = True, fill = False, padding = 5 }
            $ widget Button [#label := "Create", on #clicked CreateClicked]
          ]
        ]

nameControl :: NewProjectModel -> Widget (Event 'NewProjectMode)
nameControl model = container
  Box
  [#orientation := OrientationVertical, #halign := AlignStart]
  [ BoxChild defaultBoxChildProperties { padding = 5 }
    $ widget Label [#label := "Name", #halign := AlignStart]
  , BoxChild defaultBoxChildProperties { expand  = True
                                       , fill    = True
                                       , padding = 5
                                       }
    $ widget
        Entry
        [ #text := (model ^. newProjectName)
        , onM #changed (fmap ProjectNameChanged . entryGetText)
        ]
  ]

frameRateControl :: NewProjectModel -> Widget (Event 'NewProjectMode)
frameRateControl model = container
  Box
  [#orientation := OrientationVertical, #halign := AlignStart]
  [ BoxChild defaultBoxChildProperties { padding = 5 }
    $ widget Label [#label := "Frame Rate", #halign := AlignStart]
  , BoxChild defaultBoxChildProperties { padding = 5 }
  $   toFrameRateChanged
  <$> numberInput [] NumberInputProperties
        { value              = fromIntegral (model ^. newProjectFrameRate)
        , range              = (15, 30)
        , step               = 1
        , digits             = 0
        }
  ]
  where toFrameRateChanged (NumberInputChanged v) = FrameRateChanged v

resolutionControl :: NewProjectModel -> Widget (Event 'NewProjectMode)
resolutionControl model = container
  Box
  [#orientation := OrientationVertical, #halign := AlignStart]
  [ BoxChild defaultBoxChildProperties { padding = 5 }
    $ widget Label [#label := "Resolution", #halign := AlignStart]
  , BoxChild defaultBoxChildProperties { padding = 5 }
  $   toResolutionChanged
  <$> selectBox
        prettyPrintResolution
        []
        SelectBoxProperties
          { selected         = model ^. newProjectResolution
          , values           = resolutions
          }
  ]
  where toResolutionChanged (SelectBoxChanged r) = ResolutionChanged r
