{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExplicitNamespaces         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
module Komposition.WindowUserInterface (test) where

import           Komposition.Prelude hiding (on)

import           Control.Monad.Indexed
import           Control.Monad.Indexed.IO
import           Control.Monad.Indexed.Trans
import           Data.Row.Records
import qualified GI.Gdk                         as Gdk
import qualified GI.GLib                        as GLib
import qualified GI.Gtk                         as Gtk
import           GI.Gtk.Declarative             (bin, container, boxChild, widget, on)
import           GI.Gtk.Declarative             as Declarative
import qualified GI.Gtk.Declarative.Bin         as Declarative
import qualified GI.Gtk.Declarative.EventSource as Declarative
import           Motor.FSM                      (type (!+), type (!-), FSM,
                                                 MonadFSM, (>>>), (>>>=))
import qualified Motor.FSM                      as FSM

class WindowUserInterface m where
  type Window m :: Type -> Type -> Type

  newWindow
    :: Declarative.BinChild window Widget
    => FSM.Name n
    -> Declarative.Bin window Declarative.Widget event
    -> FSM.Actions m '[ n !+ Window m window event] r ()

  patchWindow
    :: Declarative.BinChild window Widget
    => HasType n (Window m window event) r
    => Modify n (Window m window event) r ~ r
    => FSM.Name n
    -> Declarative.Bin window Declarative.Widget event
    -> m r r ()

  setTransientFor
    :: HasType child (Window m w1 e1) r
    => HasType parent (Window m w2 e2) r
    => FSM.Name child
    -> FSM.Name parent
    -> m r r ()

  destroyWindow
    :: FSM.Name n
    -> FSM.Actions m '[ n !- Window m w e] r ()

  nextEvent
    :: HasType n (Window m w e) r
    => FSM.Name n
    -> m r r e

newtype GtkUserInterface m i o a = GtkUserInterface
  (FSM m i o a) deriving (IxFunctor, IxPointed, IxApplicative, IxMonad, MonadFSM, IxMonadTrans)

runGtkUserInterface :: Monad m => GtkUserInterface m Empty Empty a -> m a
runGtkUserInterface (GtkUserInterface a) = FSM.runFSM a

instance MonadIO m => IxMonadIO (GtkUserInterface m) where
  iliftIO = ilift . liftIO

deriving instance Monad m => Functor (GtkUserInterface m i i)
deriving instance Monad m => Applicative (GtkUserInterface m i i)
deriving instance Monad m => Monad (GtkUserInterface m i i)

data GtkWindow window event = GtkWindow
  { declarativeWidget :: Declarative.Bin window Declarative.Widget event
  , gtkWidget         :: Gtk.Widget
  , events            :: Chan event
  }

runUI :: IO a -> IO a
runUI f = do
  ret <- newEmptyMVar
  void . Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
    f >>= putMVar ret
    return False
  takeMVar ret

irunUI :: IxMonadIO m => IO a -> m i i a
irunUI = iliftIO . runUI

asGtkWindow :: GtkWindow window event -> IO Gtk.Window
asGtkWindow (GtkWindow _ w _) = Gtk.unsafeCastTo Gtk.Window w

instance MonadIO m => WindowUserInterface (GtkUserInterface m) where
  type Window (GtkUserInterface m) = GtkWindow

  newWindow name decl =
    FSM.new name =<<< irunUI (do
      w <- Declarative.create decl
      events' <- newChan
      _ <- Declarative.subscribe decl w (writeChan events')
      #showAll w
      return (GtkWindow decl w events'))

  patchWindow name decl =
    FSM.get name >>>= \w ->
      FSM.enter name =<<<
        case Declarative.patch (declarativeWidget w) decl of
          Modify f -> irunUI $ do
            f (gtkWidget w)
            return w
          Replace create' -> irunUI $ do
            Gtk.widgetDestroy (gtkWidget w)
            gtkWidget' <- create'
            events' <- newChan
            _ <- Declarative.subscribe decl gtkWidget' (writeChan events')
            return (GtkWindow decl gtkWidget' events')
          Keep -> return w

  destroyWindow name =
    FSM.get name >>>= \w ->
      irunUI (Gtk.widgetDestroy (gtkWidget w)) >>> FSM.delete name

  nextEvent name =
    FSM.get name >>>= (iliftIO . readChan . events)

  setTransientFor childName parentName =
    FSM.get childName >>>= \child' ->
      FSM.get parentName >>>= \parent -> irunUI $ do
        childWindow <- asGtkWindow child'
        parentWindow <- asGtkWindow parent
        Gtk.windowSetTransientFor childWindow (Just parentWindow)

data MainEvent = ExitRequested | ShowDetailsClicked

data DetailEvent = DetailClose

data ConfirmEvent = Yes | No

test :: IO ()
test = do
  void (Gtk.init Nothing)
  fsm <- async (runGtkUserInterface app >> Gtk.mainQuit)
  Gtk.main
  cancel fsm

  where
    app =
      newWindow #main (mainView "Welcome!")
      >>> mainLoop

    mainLoop =
      nextEvent #main >>>= \case
        ShowDetailsClicked -> showDetails >>> mainLoop
        ExitRequested -> confirmExit >>>= \case
          True -> destroyWindow #main
          False ->
            patchWindow #main (mainView "Thanks for staying around!") 
            >>> mainLoop

    confirmExit =
      newWindow #confirm (confirmDialogView "Exit?")
      >>> setTransientFor #confirm #main
      >>> nextEvent #confirm >>>= \case
        Yes -> destroyWindow #confirm >>> ireturn True
        No -> destroyWindow #confirm >>> ireturn False

    showDetails =
          newWindow #details detailView
          >>> setTransientFor #details #main
          >>> nextEvent #details >>>= \case
            DetailClose -> 
              patchWindow #main (mainView "Guess those details weren't that great, huh?") 
              >>> destroyWindow #details

mainView :: Text -> Declarative.Bin Gtk.Window Declarative.Widget MainEvent
mainView msg =
  bin Gtk.Window [#title := "Main View", on #deleteEvent (const (True, ExitRequested)), #widthRequest := 400, #heightRequest := 300] $
    container Gtk.Box [#orientation := Gtk.OrientationVertical] $ do
      boxChild True True 10 $
        widget Gtk.Label [#label := msg]
      boxChild False False 10 $
        widget Gtk.Button [#label := "Show Details", on #clicked ShowDetailsClicked]

detailView :: Declarative.Bin Gtk.Window Declarative.Widget DetailEvent
detailView =
  bin Gtk.Window [#title := "Details", #modal := True, on #deleteEvent (const (True, DetailClose)), #widthRequest := 400, #heightRequest := 300] $
    container Gtk.Box [#orientation := Gtk.OrientationVertical] $ do
      boxChild True True 10 $
        widget Gtk.Label [#label := "Details about something..."]
      boxChild False False 10 $
        widget Gtk.Button [#label := "OK", on #clicked DetailClose]

confirmDialogView :: Text -> Declarative.Bin Gtk.Window Declarative.Widget ConfirmEvent
confirmDialogView msg =
  bin Gtk.Window [#title := "Confirm", #modal := True, #widthRequest := 200, #heightRequest := 100, on #deleteEvent (const (True, No)) ] $
    container Gtk.Box [#orientation := Gtk.OrientationVertical] $ do
      boxChild False False 10 $
        widget Gtk.Label [#label := msg]
      boxChild False False 10 $
        container Gtk.Box [] $ do
          boxChild True True 10 $
            widget Gtk.Button [#label := "No", on #clicked No]
          boxChild True True 10 $
            widget Gtk.Button [#label := "Yes", on #clicked Yes]
