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
                                                                     Entry (..),
                                                                     Label (..),
                                                                     Orientation (..),
                                                                     Window (..),
                                                                     entryGetText)
import           GI.Gtk.Declarative                                 as Gtk

import           Komposition.UserInterface                          hiding
                                                                     (Window,
                                                                     newProjectView)
import           Komposition.UserInterface.GtkInterface.NumberInput
import           Komposition.UserInterface.GtkInterface.SelectBox
import           Komposition.VideoSettings

newProjectView :: NewProjectModel -> Bin Window Widget (Event NewProjectMode)
newProjectView model =
  bin Window
      [#title := "New Project", on #deleteEvent (const (True, WindowClosed))]
    $ container
        Box
        [classes ["new-project-view"], #orientation := OrientationVertical]
        [ boxChild False False 10 $ container
          Box
          [#orientation := OrientationHorizontal]
          [boxChild True True 10 $ nameControl model]
        , boxChild True True 10 $ container
          Box
          [#orientation := OrientationHorizontal, #valign := AlignStart]
          [ boxChild True True 10 $ frameRateControl model
          , boxChild True True 10 $ resolutionControl model
          ]
        , boxChild False False 10 $ container
          Box
          [#orientation := OrientationHorizontal, #halign := AlignEnd]
          [ boxChild True False 5
            $ widget Button [#label := "Cancel", on #clicked WindowClosed]
          , boxChild True False 5
            $ widget Button [#label := "Create", on #clicked CreateClicked]
          ]
        ]

nameControl :: NewProjectModel -> Widget (Event NewProjectMode)
nameControl model = container
  Box
  [#orientation := OrientationVertical, #halign := AlignStart]
  [ boxChild False False 5
    $ widget Label [#label := "Name", #halign := AlignStart]
  , boxChild True True 5 $ widget
    Entry
    [ #text := (model ^. newProjectName)
    , onM #changed (fmap ProjectNameChanged . entryGetText)
    ]
  ]

frameRateControl :: NewProjectModel -> Widget (Event NewProjectMode)
frameRateControl model = container
  Box
  [#orientation := OrientationVertical, #halign := AlignStart]
  [ boxChild False False 5
    $ widget Label [#label := "Frame Rate", #halign := AlignStart]
  , boxChild False False 5 $ toFrameRateChanged <$> numberInput
    NumberInputProperties
      { value              = fromIntegral (model ^. newProjectFrameRate)
      , range              = (15, 30)
      , step               = 1
      , digits             = 0
      , numberInputClasses = []
      }
  ]
  where toFrameRateChanged (NumberInputChanged v) = FrameRateChanged v

resolutionControl :: NewProjectModel -> Widget (Event NewProjectMode)
resolutionControl model = container
  Box
  [#orientation := OrientationVertical, #halign := AlignStart]
  [ boxChild False False 5
    $ widget Label [#label := "Resolution", #halign := AlignStart]
  , boxChild False False 5 $ toResolutionChanged <$> selectBox
    prettyPrintResolution
    SelectBoxProperties
      { selected         = model ^. newProjectResolution
      , values           = resolutions
      , selectBoxClasses = []
      }
  ]
  where toResolutionChanged (SelectBoxChanged r) = ResolutionChanged r
