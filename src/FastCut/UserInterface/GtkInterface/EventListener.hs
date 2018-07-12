{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module FastCut.UserInterface.GtkInterface.EventListener where

import           FastCut.Prelude

import qualified Data.HashMap.Strict  as HashMap
import qualified Data.HashSet         as HashSet
import qualified GI.Gdk               as Gdk
import qualified GI.GObject.Functions as GObject
import           GI.Gtk.Declarative   as Gtk

import           FastCut.KeyMap

data EventListener e = EventListener
  { events      :: Chan e
  , unsubscribe :: IO ()
  }

readEvent :: EventListener e -> IO e
readEvent = readChan . events

mergeEvents :: EventListener e -> EventListener e -> IO (EventListener e)
mergeEvents a b = do
  c <- newChan
  ta <- readInto c a
  tb <- readInto c b
  pure
    EventListener
    { events = c
    , unsubscribe =
        do killThread ta
           killThread tb
           unsubscribe a
           unsubscribe b
    }
  where
    readInto c el = forkIO (forever (readEvent el >>= writeChan c))

subscribeKeyEvents :: Gtk.Window -> IO (EventListener KeyCombo)
subscribeKeyEvents w = do
  events <- newChan
  sid <-
    w `Gtk.onWidgetKeyPressEvent` \eventKey -> do
      keyVal <- Gdk.getEventKeyKeyval eventKey
      keyChar <- toEnum . fromIntegral <$> Gdk.keyvalToUnicode keyVal
      case toKeyCombo (keyChar :: Char, keyVal) of
        Just keyCombo ->
          writeChan events (HashSet.fromList keyCombo) $> False
        _ -> return False
  return
    EventListener {unsubscribe = GObject.signalHandlerDisconnect w sid, ..}
  where
    toKeyCombo =
      \case
        (_, Gdk.KEY_Return) -> Just [KeyEnter]
        (c, _) -> Just [KeyChar c]

applyKeyMap :: KeyMap a -> EventListener KeyCombo -> IO (EventListener a)
applyKeyMap topKeyMap keyPresses = do
  xs <- newChan
  let go (KeyMap km) = do
        combo <- readChan (events keyPresses)
        case HashMap.lookup combo km of
          Just (SequencedMappings km') -> go km'
          Just (Mapping x)             -> writeChan xs x
          Nothing                      -> go (KeyMap km)
  tid <- forkIO (go topKeyMap)
  pure EventListener {events = xs, unsubscribe = killThread tid}
