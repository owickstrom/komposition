{-# LANGUAGE OverloadedStrings #-}

import           Data.GI.Base
import           Data.GI.Base.Properties
import           Data.Maybe
import           Data.Text
import           GI.GObject
import qualified GI.Gtk                  as Gtk
import           GI.Gtk.Objects.Button
import           GI.Gtk.Objects.Window   (windowResize)

import           Paths_fastcut

main :: IO ()
main = do
  Gtk.init Nothing

  gladeFile         <- getDataFileName "gui.glade"
  builder           <- Gtk.builderNewFromFile (pack gladeFile)

  window            <- builderGetObject Gtk.Window builder "window"
  fileChooserButton <- builderGetObject Gtk.Button builder "file-chooser-button"

  window `Gtk.onWidgetDestroy` Gtk.mainQuit

  windowResize window 640      480



  fileChooserButton `Gtk.onButtonClicked` do
    Gtk.widgetSetSensitive fileChooserButton False
    Gtk.buttonSetLabel fileChooserButton "Yey!"

  Gtk.widgetShowAll window

  Gtk.main

builderGetObject
  :: (GI.GObject.GObject b, Gtk.IsBuilder a)
  => (Data.GI.Base.ManagedPtr b -> b)
  -> a
  -> Prelude.String
  -> IO b
builderGetObject objectTypeClass builder objectId =
  fromJust
    <$> Gtk.builderGetObject builder (pack objectId)
    >>= Gtk.unsafeCastTo objectTypeClass
