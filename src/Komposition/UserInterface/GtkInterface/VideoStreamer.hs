{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Komposition.UserInterface.GtkInterface.VideoStreamer
  ( DesiredState(..)
  , StreamURI
  , StreamerEvent(..)
  , StreamerProperties(..)
  , videoStreamer
  )
where

import           Komposition.Prelude
import qualified Prelude

import qualified Data.GI.Base.Properties        as GI
import qualified GI.Gdk                         as Gdk
import qualified GI.GLib.Constants              as GLib
import qualified GI.GLib.Functions              as GLib
import qualified GI.Gst                         as Gst
import           GI.Gtk                         (AttrOp (..))
import qualified GI.Gtk                         as Gtk
import           GI.Gtk.Declarative             (CustomPatch (..),
                                                 CustomWidget (..), Widget (..))
import           GI.Gtk.Declarative.EventSource

type StreamURI = Text

data StreamerEvent = StreamerPlaybackEnd
  deriving (Eq, Show, Typeable)

data DesiredState = DesiredPlaying | DesiredPaused
  deriving (Eq, Show, Typeable)

data StreamerProperties = StreamerProperties
  { desiredState :: DesiredState
  , uri          :: Maybe StreamURI
  } deriving (Eq, Show, Typeable)

data StreamerState = StreamerState
  { playbin     :: Gst.Element
  , playbinBus  :: Gst.Bus
  , background  :: Gtk.Box
  , statusLabel :: Gtk.Label
  }

type StreamerTopWidget = Gtk.Box

videoStreamer :: StreamerProperties -> Widget StreamerEvent
videoStreamer customParams = Widget
  (CustomWidget
   { customWidget = Gtk.Box
   , customAttributes = mempty
   , customParams = customParams
   , customCreate = createStreamer

  -- TODO: Implement patch
   , customPatch = \old new state' ->
       if old == new
       then CustomKeep
       else CustomModify $ \_widget -> do
         void $ GI.setObjectPropertyString (playbin state') "uri" (uri new)
         return state'

    -- Modify $ \widget -> do
    --   playbin <- Gst.Element <$> GI.objectGetData widget "user_data"
    --   void $ GI.setObjectPropertyString playbin "uri" videoStreamURI
    --   case videoStreamURI of
    --     Just uri -> void $ do
    --       Gst.elementSetState playbin Gst.StatePlaying
    --     Nothing  -> return ()
    --   return ()

  , customSubscribe = \_props StreamerState{..} _widget cb -> do
      void $ Gst.busAddWatch playbinBus GLib.PRIORITY_DEFAULT $ \_bus msg -> do
        msgType <- Gst.getMessageType msg
        case msgType of
          [Gst.MessageTypeError] -> do
            (gError, _) <- Gst.messageParseError msg
            gErrorText  <- Gst.gerrorMessage gError
            code        <- Gst.gerrorCode gError
            domain      <- GLib.quarkToString =<< Gst.gerrorDomain gError
            case (domain, code) of
              ("gst-resource-error-quark", 5) -> do
                -- In case the HTTP server for the stream is not
                -- yet up, this error will be thrown, and we
                -- restart the GStreamer playbin after 500ms.
                let delayMs = 500
                putStrLn ("HTTP stream not yet available, restarting in " <> show delayMs <> " ms..." :: Text)
                threadDelay (delayMs * 1000)
                runUI_ $ do
                    void . Gst.elementSetState playbin $ Gst.StateNull
                    void . Gst.elementSetState playbin $ Gst.StatePlaying
              _ -> liftIO . putStrLn $ domain <> " - " <> show gError <> ": " <> gErrorText -- TODO: emit event
          [Gst.MessageTypeBuffering] ->
            Gst.messageParseBuffering msg >>= \case
              percent
                | percent >= 100 -> do
                    -- No more buffering needed, hide overlay and
                    -- continue playing
                    #hide background
                    void $ Gst.elementSetState playbin Gst.StatePlaying
                | otherwise -> do
                    -- Buffering needed, show overlay with
                    -- appropriate text and paus the playbin
                    #show background
                    Gtk.labelSetLabel statusLabel ("Buffering (" <> show percent <> "%)")
                    void $ Gst.elementSetState playbin Gst.StatePaused
          [Gst.MessageTypeEos] -> cb StreamerPlaybackEnd
          -- Commented out, but useful for debugging:
          --
          -- [Gst.MessageTypeStateChanged] -> do
          --   (oldState, newState, _) <- Gst.messageParseStateChanged msg
          --   putStrLn ("State changed: " <> show oldState <> " -> " <> show newState :: Text)
          --   return ()
          -- [Gst.MessageTypeStreamStatus] -> do
          --   (statusType, _owner) <- Gst.messageParseStreamStatus msg
          --   putStrLn ("Stream status: " <> show statusType :: Text)
          --   return ()
          _                    -> pass
        pure True
      return (fromCancellation (void (Gst.busRemoveWatch playbinBus)))
  })


createStreamer :: StreamerProperties -> IO (StreamerTopWidget, StreamerState)
createStreamer StreamerProperties {..} = do
  container                          <- Gtk.new Gtk.Box []
  (overlay, background, statusLabel) <- createOverlayAndLabel container
  playbin                            <-
    Gst.elementFactoryMake "playbin" Nothing `orFailCreateWith` "playbin"
  playbinBus <- Gst.elementGetBus playbin `orFailCreateWith` "playbin bus"
  gtkSink    <-
    Gst.elementFactoryMake "gtksink" Nothing `orFailCreateWith` "GTK sink"
  GI.setObjectPropertyObject playbin "video-sink" (Just gtkSink)
  GI.setObjectPropertyBool playbin "force-aspect-ratio" True

  videoWidget <-
    GI.getObjectPropertyObject gtkSink "widget" Gtk.Widget
      `orFailCreateWith` "sink widget"
  GI.setObjectPropertyBool playbin "force-aspect-ratio" True
  void $ GI.setObjectPropertyString playbin "uri" uri

  -- let updateProgress = do
  --       forever (void await)
  -- ffmpegRenderer <- async $ runSafeT (runEffect (streamingProcess >-> updateProgress))

  void . Gtk.onWidgetRealize container $ do
    #add overlay videoWidget
    #showAll overlay
    -- #setSizeRequest
    --   videoWidget
    --   (fromIntegral (videoSettings ^. resolution . width))
    --   (fromIntegral (videoSettings ^. resolution . height))

    -- Try start streaming
    void $ Gst.elementSetState playbin Gst.StatePlaying

  void . Gtk.onWidgetDestroy container $
    void $ Gst.elementSetState playbin Gst.StateNull
    -- void . async $ cancel ffmpegRenderer

  pure (container, StreamerState {..})

createOverlayAndLabel :: Gtk.Box -> IO (Gtk.Overlay, Gtk.Box, Gtk.Label)
createOverlayAndLabel content = do
  overlay <- Gtk.new Gtk.Overlay []
  background <- Gtk.new Gtk.Box []
  style <- Gtk.widgetGetStyleContext background
  Gtk.styleContextAddClass style "preview-overlay"
  statusLabel <- Gtk.new Gtk.Label [#label := "Initializing..."]
  #packStart background statusLabel True True 0
  #addOverlay overlay background
  #packStart content overlay True True 0
  pure (overlay, background, statusLabel)

orFailCreateWith :: IO (Maybe t) -> Prelude.String -> IO t
orFailCreateWith action what =
  action >>=
  maybe
    (Prelude.fail ("Couldn't create GStreamer " <> what <> "."))
    return

runUI_ :: IO () -> IO ()
runUI_ ma = void (Gdk.threadsAddIdle GLib.PRIORITY_HIGH (ma *> return False))
