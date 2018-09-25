{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module FastCut.UserInterface.GtkInterface.ThumbnailPreview where

import           FastCut.Prelude

import           Control.Lens
import qualified GI.Gdk                         as Gdk
import qualified GI.GdkPixbuf                   as Pixbuf
import qualified GI.GLib                        as GLib
import qualified GI.Gtk                         as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.EventSource

import           FastCut.VideoSettings

data ThumbnailPreview event = ThumbnailPreview
  { thumbnailPath :: FilePath
  , initialSize   :: Resolution
  }

thumbnailPreview :: FilePath -> Resolution -> Widget a
thumbnailPreview thumbnailPath initialSize =
  Widget ThumbnailPreview {..}

instance Functor ThumbnailPreview where
  fmap f ThumbnailPreview {..} = ThumbnailPreview {..}

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
  -- TODO: Implement patch
  patch _ ThumbnailPreview {..} =
    Keep

instance EventSource ThumbnailPreview where
  subscribe ThumbnailPreview{..} _ _ = do
    -- TODO: Implement event source
    return (fromCancellation (return ()))
