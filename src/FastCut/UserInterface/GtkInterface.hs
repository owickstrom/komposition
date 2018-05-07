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
import           Data.Row.Records                                (Empty)
import           Data.String
import qualified Data.Text                                       as Text
import qualified GI.Gdk                                          as Gdk
import qualified GI.GLib.Constants                               as GLib
import           Motor.FSM                                       as FSM

import           Control.Monad.Indexed.IO
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
  GtkTimelineMode :: SharedState -> GtkInterfaceState TimelineMode
  GtkLibraryMode :: SharedState -> GtkInterfaceState LibraryMode

runUI :: IO () -> IO ()
runUI f = void (Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT (f *> return False))

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
  return (SharedState window obj)
  where
    cssPriority = fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER

render :: IxMonadIO m => SharedState -> Object -> m i i SharedState
render s@SharedState {..} newObj =
  iliftIO $ do
    runUI (patchBox window currentObject newObj)
    return s { currentObject = newObj }
  where
    patchBox :: Gtk.Window -> Object -> Object -> IO ()
    patchBox w o1 o2 =
      case patch o1 o2 of
        Modify f ->
          Gtk.containerGetChildren w >>= \case
            [] -> return ()
            (c:_) -> f =<< Gtk.toWidget c
        Replace createNew -> do
          Gtk.containerForall w (Gtk.containerRemove w)
          newWidget <- createNew
          Gtk.containerAdd w newWidget
          Gtk.widgetShowAll w
        Keep -> return ()

instance (MonadReader Env m, MonadIO m) => UserInterface (GtkInterface m) where
  type State (GtkInterface m) = GtkInterfaceState
  start n project focus =
    ilift ask >>>= \env ->
      iliftIO (initializeWindow env (timelineView project focus))
      >>>= FSM.new n . GtkTimelineMode
  updateTimeline n project focus =
    FSM.get n >>>= \(GtkTimelineMode s) ->
      render s (timelineView project focus) >>>= (FSM.enter n . GtkTimelineMode)
  enterLibrary n lib clipType idx =
    FSM.get n >>>= \(GtkTimelineMode s) ->
      render s (libraryView lib clipType idx) >>>=
      (FSM.enter n . GtkLibraryMode)
  nextEvent n =
    FSM.get n >>>= \case
      GtkTimelineMode {} -> ireturn OpenLibrary
      GtkLibraryMode {} -> ireturn LibraryEscape
  exitLibrary n project focus =
    FSM.get n >>>= \(GtkLibraryMode s) ->
      render s (timelineView project focus) >>>= (FSM.enter n . GtkTimelineMode)
  exit n = iliftIO Gtk.mainQuit >>> delete n

run :: FilePath -> GtkInterface (ReaderT Env IO) Empty Empty () -> IO ()
run cssPath ui = do
  void $ Gtk.init Nothing
  screen <- maybe (fail "No screen?!") return =<< Gdk.screenGetDefault
  -- Start the application in a separate thread.
  void (forkIO (runReaderT (runFSM (runGtkInterface ui)) Env {..}))
  Gtk.main
