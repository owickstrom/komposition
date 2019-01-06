{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | The library view of Komposition's GTK interface.
module Komposition.UserInterface.GtkInterface.LibraryView
  ( libraryView
  ) where

import           Komposition.Prelude       hiding (State, on)
import qualified Prelude

import           Control.Lens
import           Data.Text                 (Text)
import qualified Data.Vector               as Vector
import           GI.Gtk                    (Box (..), Button (..), Label (..),
                                            ListBox (..), ListBoxRow (..),
                                            Orientation (..), PolicyType (..),
                                            ScrolledWindow (..),
                                            SelectionMode (..), Window (..))
import           GI.Gtk.Declarative
import           System.FilePath
import           Text.Printf

import           Komposition.Duration
import           Komposition.Library
import           Komposition.MediaType
import           Komposition.UserInterface hiding (Window, libraryView)

printTimestamp :: Duration -> Text
printTimestamp d =
  let
    sec = durationToSeconds d
    hours, minutes :: Int
    hours   = floor (sec / 3600)
    minutes = floor (sec / 60)
    seconds :: Double
    seconds = sec - (fromIntegral hours * 3600) - (fromIntegral minutes * 60)
    leadingZero = if seconds < 10 then "0" else "" :: Prelude.String
    secondsStr = printf "%s%.2f" leadingZero seconds :: Prelude.String
  in
    toS (printf "%02d:%02d:%s" hours minutes secondsStr :: Prelude.String)

renderVideoAsset ::
     VideoAsset -> Bin ListBoxRow (Event LibraryMode)
renderVideoAsset asset' =
  bin ListBoxRow [on #activate LibrarySelectionConfirmed]
    $ case asset' ^. videoClassifiedScene of
        Just (n, timeSpan) -> container
          Box
          [classes ["video", "classified-scene"]]
          [ let lbl :: Prelude.String
                lbl = printf
                  "%s (%d)"
                  (takeFileName
                    (asset' ^. assetMetadata . path . unOriginalPath)
                  )
                  n
            in  BoxChild defaultBoxChildProperties { expand = True, fill = False, padding = 0 } $ widget Label [#label := toS lbl]
          , BoxChild defaultBoxChildProperties { expand = False, fill = False, padding = 10 }
            $ widget
                Label
                [ #label := printTimestamp
                    (durationOf OriginalDuration timeSpan)
                ]
          , BoxChild defaultBoxChildProperties { expand = False, fill = False, padding = 10 }
            $ widget
                Label
                [ #label := printTimestamp
                    (durationOf AdjustedDuration timeSpan)
                ]
          ]
        Nothing -> container
          Box
          [classes ["video"]]
          [ BoxChild defaultBoxChildProperties { expand = True, fill = False, padding = 0 } $ widget
            Label
            [ #label := toS
                (takeFileName (asset' ^. assetMetadata . path . unOriginalPath))
            ]
          , BoxChild defaultBoxChildProperties { expand = False, fill = False, padding = 10 }
            $ widget
                Label
                [#label := printTimestamp (asset' ^. assetMetadata . duration)]
          ]

renderAudioAsset ::
     AudioAsset -> Bin ListBoxRow (Event LibraryMode)
renderAudioAsset asset' =
  bin ListBoxRow [on #activate LibrarySelectionConfirmed] $ container
    Box
    [classes ["audio"]]
    [ BoxChild defaultBoxChildProperties { expand  = True
                                         , fill    = False
                                         , padding = 0
                                         }
      $ widget
          Label
          [ #label := toS
              (takeFileName (asset' ^. assetMetadata . path . unOriginalPath))
          ]
    , BoxChild defaultBoxChildProperties { expand  = False
                                         , fill    = False
                                         , padding = 10
                                         }
      $ widget
          Label
          [#label := printTimestamp (asset' ^. assetMetadata . duration)]
    ]

libraryView ::  SelectAssetsModel mt -> Bin Window (Event LibraryMode)
libraryView SelectAssetsModel {..} =
  bin
      Window
      [ #title := "Library"
      , on #deleteEvent (const (True, WindowClosed))
      , #defaultWidth := 300
      , #defaultHeight := 400
      , classes ["library"]
      ]
    $ container
        Box
        [#orientation := OrientationVertical]
        [ BoxChild defaultBoxChildProperties { expand  = True
                                             , fill    = True
                                             , padding = 0
                                             }
          $ bin
              ScrolledWindow
              [ #hscrollbarPolicy := PolicyTypeNever
              , #vscrollbarPolicy := PolicyTypeAutomatic
              ]
              assetSelectList
        , BoxChild defaultBoxChildProperties { expand  = False
                                             , fill    = False
                                             , padding = 10
                                             }
          $ container
              Box
              []
              [ BoxChild defaultBoxChildProperties { expand  = True
                                                   , fill    = True
                                                   , padding = 10
                                                   }
                $ widget
                    Button
                    [ #label := "Cancel"
                    , on #clicked (CommandKeyMappedEvent Cancel)
                    ]
              , BoxChild defaultBoxChildProperties { expand  = True
                                                   , fill    = True
                                                   , padding = 10
                                                   }
                $ widget
                    Button
                    [ #label := "Select"
                    , #sensitive := not (null selectedAssets)
                    , on #clicked LibrarySelectionConfirmed
                    ]
              ]
        ]
  where
    assetSelectList =
      container
          ListBox
          [ #selectionMode := SelectionModeMultiple
          , onM #selectedRowsChanged emitAssetsSelected
          , on #activateCursorRow LibrarySelectionConfirmed
          , classes ["clips"]
          ]
        $ case mediaType of
            SVideo -> Vector.fromList . toList $ renderVideoAsset <$> allAssets
            SAudio -> Vector.fromList . toList $ renderAudioAsset <$> allAssets
    emitAssetsSelected listBox = do
      is <- map fromIntegral <$> (#getSelectedRows listBox >>= mapM #getIndex)
      let selected = allAssets ^.. traversed . indices (`elem` is)
      return (LibraryAssetsSelected mediaType selected)
