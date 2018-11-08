{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE GADTs   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Komposition.UserInterface.GtkInterface.ThumbnailPreview where

import           Komposition.Prelude

import Data.IORef
import qualified GI.Gdk                         as Gdk
import qualified GI.GdkPixbuf                   as Pixbuf
import qualified GI.GLib                        as GLib
import qualified GI.Gtk                         as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.State

import Komposition.UserInterface.GtkInterface.CustomWidget

type CustomState = IORef FilePath

thumbnailPreview :: FilePath -> Widget a
thumbnailPreview customData = Widget (CustomWidget {..})
  where
    customWidget = Gtk.Layout
    customCreate thumbnailPath = do
      src <- newIORef thumbnailPath
      layout <- Gtk.layoutNew Gtk.noAdjustment Gtk.noAdjustment
      sc <- Gtk.widgetGetStyleContext layout
      image <- Gtk.imageNewFromPixbuf Pixbuf.noPixbuf
      Gtk.widgetSetSizeRequest layout 200 200
      Gtk.layoutPut layout image 0 0
      Gtk.widgetShowAll layout
      let redraw w h = void . async $ do
            srcPath <- readIORef src
            scaled <- Pixbuf.pixbufNewFromFileAtSize srcPath w h
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
      return (SomeState (StateTreeWidget (StateTreeNode layout sc mempty src)))

    customPatch (SomeState (stateTree :: StateTree stateType w e c cs)) old (new :: FilePath)
      | old == new = CustomKeep
      | otherwise =
        case (stateTree, eqT @cs @CustomState) of
          (StateTreeWidget top, Just Refl) -> CustomModify $ \_ -> do
            writeIORef (stateTreeCustomState top) new
            return (SomeState (StateTreeWidget top))

          _ -> CustomReplace

    customSubscribe _ _ _ =
      return (fromCancellation (return ()))
