{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Komposition.UserInterface.GtkInterface.ThumbnailPreview where

import           Komposition.Prelude

import qualified GI.Gdk                         as Gdk
import qualified GI.GdkPixbuf                   as Pixbuf
import qualified GI.GLib                        as GLib
import qualified GI.Gtk                         as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.EventSource

data ThumbnailPreview event = ThumbnailPreview
  { thumbnailPath :: FilePath
  }

thumbnailPreview :: FilePath -> Widget a
thumbnailPreview thumbnailPath =
  Widget ThumbnailPreview {..}

instance Functor ThumbnailPreview where
  fmap _ ThumbnailPreview {..} = ThumbnailPreview {..}

instance Patchable ThumbnailPreview where
  create ThumbnailPreview {..} = do
    layout <- Gtk.layoutNew Gtk.noAdjustment Gtk.noAdjustment
    image <- Gtk.imageNewFromPixbuf Pixbuf.noPixbuf
    Gtk.widgetSetSizeRequest layout 200 200
    Gtk.layoutPut layout image 0 0
    let redraw w h = void . async $ do
          scaled <- Pixbuf.pixbufNewFromFileAtSize thumbnailPath w h
          void . Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
            Gtk.imageSetFromPixbuf image (Just scaled)
            return False
    void . Gtk.onWidgetMapEvent layout $ \_ -> do
      (_, r) <- Gtk.widgetGetPreferredSize layout
      w <- Gtk.getRequisitionWidth r
      h <- Gtk.getRequisitionHeight r
      redraw w h
      return True
    void . Gtk.onWidgetSizeAllocate layout $ \a -> do
      w <- Gdk.getRectangleWidth a
      h <- Gdk.getRectangleHeight a
      redraw w h
    Gtk.toWidget layout
  patch old new
    | thumbnailPath old == thumbnailPath new = Keep
    | otherwise = Replace (create new)

instance EventSource ThumbnailPreview where
  subscribe ThumbnailPreview{..} _ _ = do
    -- TODO: Implement event source
    return (fromCancellation (return ()))
