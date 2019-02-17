{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Komposition.UserInterface.GtkInterface.WelcomeScreenView
  ( welcomeScreenView
  )
where

import           Komposition.Prelude       hiding (on)

import           GI.Gtk                    (Box (..), Button (..), Label (..),
                                            Orientation (..), Window (..))
import           GI.Gtk.Declarative

import           Komposition.UserInterface (Event (..), Mode (..))

welcomeScreenView :: Bin Window (Event 'WelcomeScreenMode)
welcomeScreenView =
  bin Window
      [#title := "Komposition", on #deleteEvent (const (True, WindowClosed))]
    $ container
        Box
        [ #orientation := OrientationVertical
        , classes ["welcome-screen"]
        , #widthRequest := 400
        , #heightRequest := 300
        ]
        [ widget Label [classes ["title"], #label := "Komposition"]
        , widget
          Label
          [ classes ["subtitle"]
          , #label := "The video editor built for screencasters"
          ]
        , container Box [] [BoxChild defaultBoxChildProperties { expand = True } actions]
        ]
  where
    actions =
      container
        Box
        [#orientation := OrientationVertical, classes ["actions"]]
        [ widget
          Button
          [ #label := "Create New Project"
          , on #clicked CreateNewProjectClicked
          ]
        , widget
          Button
          [ #label := "Open Existing Project"
          , on #clicked OpenExistingProjectClicked
          ]
        ]
