{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | The library view of FastCut's GTK interface.
module FastCut.UserInterface.GtkInterface.LibraryView
  ( libraryView
  ) where

import           FastCut.Prelude       hiding (State, on)
import qualified Prelude

import           Control.Lens
import           Data.Text             (Text)
import           GI.Gtk                (Box (..), Button (..), Label (..),
                                        ListBox (..), ListBoxRow (..),
                                        Orientation (..), PolicyType (..),
                                        ScrolledWindow (..), SelectionMode (..),
                                        Window (..))
import           GI.Gtk.Declarative
import           System.FilePath
import           Text.Printf

import           FastCut.Duration
import           FastCut.Library
import           FastCut.MediaType
import           FastCut.UserInterface

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
     VideoAsset -> MarkupOf (Bin ListBoxRow Widget) (Event LibraryMode) ()
renderVideoAsset asset' =
  bin ListBoxRow [on #activate LibrarySelectionConfirmed] $
  case asset' ^. videoClassifiedScene of
    Just (n, timeSpan) ->
      container Box [classes ["video", "classified-scene"]] $ do
        let lbl :: Prelude.String
            lbl = printf "%s (%d)" (takeFileName (asset' ^. assetMetadata . path)) n
        boxChild True False 0 $ widget Label [#label := toS lbl]
        boxChild False False 10 $
          widget Label [#label := printTimestamp (durationOf timeSpan)]
    Nothing ->
      container Box [classes ["video"]] $ do
        boxChild True False 0 $
          widget
            Label
            [#label := toS (takeFileName (asset' ^. assetMetadata . path))]
        boxChild False False 10 $
          widget
            Label
            [#label := printTimestamp (asset' ^. assetMetadata . duration)]

renderAudioAsset ::
     AudioAsset -> MarkupOf (Bin ListBoxRow Widget) (Event LibraryMode) ()
renderAudioAsset asset' =
  bin ListBoxRow [on #activate LibrarySelectionConfirmed] $
  container Box [classes ["audio"]] $ do
    boxChild True False 0 $
      widget
        Label
        [#label := toS (takeFileName (asset' ^. assetMetadata . path))]
    boxChild False False 10 $
      widget
        Label
        [#label := printTimestamp (asset' ^. assetMetadata . duration)]

libraryView ::  SelectAssetsModel mt -> Widget (Event LibraryMode)
libraryView SelectAssetsModel {..} =
  bin
    Window
    [ #title := "Library"
    , on #destroy (CommandKeyMappedEvent Cancel)
    , #defaultWidth := 300
    , #defaultHeight := 400
    , classes ["library"]
    ] $
  container Box [#orientation := OrientationVertical] $ do
    boxChild True True 0 $
      bin
        ScrolledWindow
        [ #hscrollbarPolicy := PolicyTypeNever
        , #vscrollbarPolicy := PolicyTypeAutomatic
        ]
        assetSelectList
    boxChild False False 10 $ container Box [] $ do
      boxChild True True 10 $
        widget
          Button
          [#label := "Cancel", on #clicked (CommandKeyMappedEvent Cancel)]
      boxChild True True 10 $
        widget
          Button
          [ #label := "Select"
          , #sensitive := not (null selectedAssets)
          , on #clicked LibrarySelectionConfirmed
          ]
  where
    assetSelectList =
      container
        ListBox
        [ #selectionMode := SelectionModeMultiple
        , onM #selectedRowsChanged emitAssetsSelected
        , on #activateCursorRow LibrarySelectionConfirmed
        , classes ["clips"]
        ] $
      case mediaType of
        SVideo -> for_ allAssets renderVideoAsset
        SAudio -> for_ allAssets renderAudioAsset
    emitAssetsSelected listBox = do
      is <- map fromIntegral <$> (#getSelectedRows listBox >>= mapM #getIndex)
      let selected = (allAssets ^.. traversed . indices (`elem` is))
      return (LibraryAssetsSelected mediaType selected)
