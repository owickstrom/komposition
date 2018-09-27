{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Komposition.UserInterface.GtkInterface.VideoStreamer where

import           Komposition.Prelude
import qualified Prelude

import qualified Data.GI.Base.Properties        as GI
import qualified GI.GLib
import qualified GI.GObject.Objects             as GI
import qualified GI.Gst                         as Gst
import qualified GI.Gtk                         as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.EventSource

type VideoStreamURI = Text

data VideoStreamerEvent = VideoPlaybackEnd

data VideoStreamer event = VideoStreamer
  { videoStreamURI :: Maybe VideoStreamURI
  , onEvent        :: VideoStreamerEvent -> event
  }

videoStreamer :: Maybe VideoStreamURI -> Widget VideoStreamerEvent
videoStreamer uri =
  Widget VideoStreamer { videoStreamURI = uri, onEvent = identity }

instance Functor VideoStreamer where
  fmap f VideoStreamer {..} = VideoStreamer {onEvent = fmap f onEvent, ..}

instance Patchable VideoStreamer where
  create VideoStreamer {..} = do
    box <- Gtk.boxNew Gtk.OrientationVertical 0
    Gtk.widgetSetSizeRequest box 400 250
    void . Gtk.onWidgetRealize box $ do
      playbin <-
        Gst.elementFactoryMake "playbin" Nothing `orFailCreateWith` "playbin"
      playbinBus <- Gst.elementGetBus playbin `orFailCreateWith` "playbin bus"
      gtkSink <-
        Gst.elementFactoryMake "gtksink" Nothing `orFailCreateWith` "GTK sink"
      GI.setObjectPropertyObject playbin "video-sink" (Just gtkSink)
      GI.setObjectPropertyBool playbin "force-aspect-ratio" True
      void . Gst.busAddWatch playbinBus GI.GLib.PRIORITY_DEFAULT $ \bus msg -> do
        print =<< Gst.getMessageType msg
        return True
      videoWidget <-
        GI.getObjectPropertyObject gtkSink "widget" Gtk.Widget `orFailCreateWith`
        "sink widget"
      win <- Gtk.widgetGetToplevel box
      void $ GI.setObjectPropertyString playbin "uri" videoStreamURI
      case videoStreamURI of
        Just uri -> void $ do
          Gst.elementSetState playbin Gst.StatePlaying
        Nothing  -> return ()
      Gtk.containerAdd box videoWidget
    Gtk.toWidget box
  -- TODO: Implement patch
  patch _ VideoStreamer {..} =
    Keep
    -- Modify $ \widget -> do
    --   playbin <- Gst.Element <$> GI.objectGetData widget "user_data"
    --   void $ GI.setObjectPropertyString playbin "uri" videoStreamURI
    --   case videoStreamURI of
    --     Just uri -> void $ do
    --       Gst.elementSetState playbin Gst.StatePlaying
    --     Nothing  -> return ()
    --   return ()

instance EventSource VideoStreamer where
  subscribe VideoStreamer{..} _ _ = do
    -- TODO: Implement event source
    return (fromCancellation (return ()))
