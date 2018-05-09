{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RebindableSyntax           #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | A declarative GTK implementation of the 'UserInterface' protocol.
module FastCut.UserInterface.GtkInterface (run) where

import           Prelude                                         hiding (log)

import           Control.Concurrent
import           Control.Monad                                   (void)
import           Control.Monad.Indexed
import           Control.Monad.Indexed.Trans
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Functor                                    (($>))
import qualified Data.GI.Base.Signals                            as GI
import qualified Data.HashSet                                    as HashSet
import           Data.Row.Records                                (Empty)
import           Data.String
import qualified Data.Text                                       as Text
import qualified GI.Gdk                                          as Gdk
import qualified GI.GLib.Constants                               as GLib
import qualified GI.GObject.Functions                            as GObject
import           Motor.FSM                                       as FSM

import           Control.Monad.Indexed.IO
import           FastCut.Focus
import           FastCut.Project
import           FastCut.Sequence
import           FastCut.UserInterface
import           FastCut.UserInterface.GtkInterface.LibraryView
import           FastCut.UserInterface.GtkInterface.TimelineView
import           GI.Gtk.Declarative                              as Gtk

data Env = Env
  { cssPath :: FilePath
  , screen  :: Gdk.Screen
  }

data SharedState = SharedState
  { window        :: Gtk.Window
  , eventListener :: EventListener
  , currentObject :: Object
  }

instance MonadIO m => IxMonadIO (GtkInterface m) where
  iliftIO = ilift . liftIO

newtype GtkInterface m i o a = GtkInterface
  { runGtkInterface :: FSM m i o a
  } deriving (IxFunctor, IxPointed, IxApplicative, IxMonad, MonadFSM, IxMonadTrans)

deriving instance Monad m => Functor (GtkInterface m i i)
deriving instance Monad m => Applicative (GtkInterface m i i)
deriving instance Monad m => Monad (GtkInterface m i i)

data GtkInterfaceState s where
  GtkTimelineMode
    :: SharedState
    -> GtkInterfaceState TimelineMode
  GtkLibraryMode
    :: SharedState
    -> GtkInterfaceState LibraryMode

sharedState :: GtkInterfaceState s -> SharedState
sharedState = \case
  GtkTimelineMode s -> s
  GtkLibraryMode s -> s

runUI :: IO () -> IO ()
runUI f = void (Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT (f *> return False))

data EventListener = EventListener
  { events          :: Chan Event
  , signalHandlerId :: GI.SignalHandlerId
  }

subscribeEvents :: Gtk.Window -> IO EventListener
subscribeEvents w = do
  events <- newChan
  signalHandlerId <-
    w `Gtk.onWidgetKeyPressEvent` \eventKey -> do
      keyVal <- Gdk.getEventKeyKeyval eventKey
      keyChar <- toEnum . fromIntegral <$> Gdk.keyvalToUnicode keyVal
      case toKeyCombo (keyChar :: Char, keyVal) of
        Just keyCombo ->
          writeChan events (KeyPress (HashSet.fromList keyCombo)) $> False
        _ -> return False
  return EventListener {..}
  where
    toKeyCombo =
      \case
        (_, Gdk.KEY_Return) -> Just [KeyEnter]
        (c, _) -> Just [KeyChar c]

unsubscribeEvents :: GtkInterfaceState s -> IO ()
unsubscribeEvents (sharedState -> s) = do
  let EventListener{..} = eventListener s
  GObject.signalHandlerDisconnect (window s) signalHandlerId

awaitNext :: GtkInterfaceState s -> IO Event
awaitNext = readChan . events . eventListener . sharedState

initializeWindow :: Env -> Object -> IO SharedState
initializeWindow Env{cssPath, screen} obj = do
  w <- newEmptyMVar
  runUI $ do
    window <- Gtk.windowNew Gtk.WindowTypeToplevel
    Gtk.windowSetTitle window "FastCut"
    Gtk.windowResize window 640 480
    void $ Gtk.onWidgetDestroy window Gtk.mainQuit
    cssProvider <- Gtk.cssProviderNew
    Gtk.cssProviderLoadFromPath cssProvider (Text.pack cssPath)
    Gtk.styleContextAddProviderForScreen screen cssProvider cssPriority
    Gtk.widgetShowAll window
    Gtk.containerAdd window =<< Gtk.toWidget =<< create obj
    Gtk.widgetShowAll window
    putMVar w window
  window <- takeMVar w
  listener <- subscribeEvents window
  return (SharedState window listener obj)
  where
    cssPriority = fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER

render :: Object -> SharedState -> IO SharedState
render newObj s@SharedState {..} = do
  runUI (patchBox window currentObject newObj)
  return s {currentObject = newObj}
  where
    patchBox :: Gtk.Window -> Object -> Object -> IO ()
    patchBox w o1 o2 =
      case patch o1 o2 of
        Modify f ->
          Gtk.containerGetChildren w >>= \case
            [] -> return ()
            (c:_) -> do
              f =<< Gtk.toWidget c
              Gtk.widgetShowAll w
        Replace createNew -> do
          Gtk.containerForall w (Gtk.containerRemove w)
          newWidget <- createNew
          Gtk.containerAdd w newWidget
          Gtk.widgetShowAll w
        Keep -> return ()

firstTimelineView ::  Project -> Focus -> Env -> IO (GtkInterfaceState TimelineMode)
firstTimelineView project focus env =
  GtkTimelineMode <$> initializeWindow env (timelineView project focus)

newTimelineView :: Project -> Focus -> GtkInterfaceState s -> IO (GtkInterfaceState TimelineMode)
newTimelineView project focus is =
  GtkTimelineMode <$> render (timelineView project focus) (sharedState is)

newLibraryView :: [Clip Focused t] -> GtkInterfaceState s -> IO (GtkInterfaceState LibraryMode)
newLibraryView clips is =
  GtkLibraryMode <$> render (libraryView clips) (sharedState is)

instance (MonadReader Env m, MonadIO m) => UserInterface (GtkInterface m) where
  type State (GtkInterface m) = GtkInterfaceState

  start n project focus =
    ilift ask
    >>>= iliftIO . firstTimelineView project focus
    >>>= FSM.new n

  -- NOTE: This re-renders (with diff) and re-attaches event listener:
  updateTimeline n project focus =
    FSM.get n
    >>>= iliftIO . newTimelineView project focus
    >>>= FSM.enter n

  enterLibrary n =
    FSM.get n
    >>>= iliftIO . newLibraryView []
    >>>= FSM.enter n

  updateLibrary n clips =
    FSM.get n
    >>>= iliftIO . newLibraryView clips
    >>>= FSM.enter n

  nextEvent n = FSM.get n >>>= iliftIO . awaitNext

  exitLibrary n project focus =
    FSM.get n
    >>>= iliftIO . newTimelineView project focus
    >>>= FSM.enter n

  beep _ = iliftIO (runUI Gdk.beep)

  exit n =
    (FSM.get n >>>= iliftIO . unsubscribeEvents)
    >>> iliftIO Gtk.mainQuit
    >>> delete n

run :: FilePath -> GtkInterface (ReaderT Env IO) Empty Empty () -> IO ()
run cssPath ui = do
  void $ Gtk.init Nothing
  screen <- maybe (fail "No screen?!") return =<< Gdk.screenGetDefault
  -- Start the application in a separate thread.
  void (forkIO (runReaderT (runFSM (runGtkInterface ui)) Env {..}))
  Gtk.main
