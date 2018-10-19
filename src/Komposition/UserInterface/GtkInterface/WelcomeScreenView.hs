{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
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

import           Komposition.UserInterface (Event(..), Mode(..))

welcomeScreenView :: Bin Window Widget (Event WelcomeScreenMode)
welcomeScreenView =
  bin Window [ #title := "Komposition"
             , on #deleteEvent (const (True, WindowClosed))
             ]
    $ container
        Box
        [ #orientation := OrientationVertical
        , classes ["welcome-screen"]
        , #widthRequest := 400
        , #heightRequest := 300
        ]
    $ do
        boxChild False False 0
          $ widget Label [classes ["title"], #label := "Komposition"]
        boxChild False False 0 $ widget
          Label
          [ classes ["subtitle"]
          , #label := "The video editor built for screencasters"
          ]
        boxChild False False 0
          $ container
              Box
              [#orientation := OrientationVertical, classes ["actions"]]
          $ do
              boxChild False False 0 $ widget
                Button
                [ #label := "Create New Project"
                , on #clicked CreateNewProjectClicked
                ]
              boxChild False False 0 $ widget
                Button
                [ #label := "Open Existing Project"
                , on #clicked OpenExistingProjectClicked
                ]
