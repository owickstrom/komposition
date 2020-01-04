{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ConstraintKinds            #-}
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
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | A declarative GTK implementation of the 'UserInterface' protocol.
module Komposition.UserInterface.GtkInterface
  ( runGtkUserInterface
  )
where

import           Komposition.Prelude                                      hiding (Reader,
                                                                           Void,
                                                                           ask,
                                                                           runReader,
                                                                           state)
import           Prelude                                                  (fail)
import qualified Prelude

import           Control.Effect
import           Control.Effect.Carrier                                   (Carrier)
import           Control.Effect.Reader                                    (ReaderC,
                                                                           ask,
                                                                           runReader)
import           Control.Monad                                            (void)
import           Control.Monad.Indexed                                    ()
import           Control.Monad.Indexed.Trans
import qualified Data.HashSet                                             as HashSet
import           Data.Row.Records                                         (Empty,
                                                                           HasType)
import           Data.String
import qualified Data.Text                                                as Text
import           Data.Time.Clock                                          (diffTimeToPicoseconds)
import qualified GI.Gdk                                                   as Gdk
import qualified GI.Gst                                                   as Gst
import           GI.Gtk                                                   (AttrOp (..))
import qualified GI.Gtk                                                   as Gtk
import qualified GI.Gtk.Declarative                                       as Declarative
import qualified GI.Gtk.Declarative.State                                 as Declarative
import           Motor.FSM                                                hiding
                                                                           ((:=))
import qualified Motor.FSM                                                as FSM
import           Pipes
import           Pipes.Safe                                               (runSafeT,
                                                                           tryP)
import           Text.Printf

import           Control.Monad.Indexed.IO
import           Komposition.KeyMap
import           Komposition.Progress
import           Komposition.UserInterface
import           Komposition.UserInterface.WindowUserInterface
import           Komposition.UserInterface.GtkInterface.EventListener
import           Komposition.UserInterface.GtkInterface.GtkWindowMarkup
import           Komposition.UserInterface.GtkInterface.Threading

import qualified Komposition.UserInterface.GtkInterface.ImportView        as View
import qualified Komposition.UserInterface.GtkInterface.LibraryView       as View
import qualified Komposition.UserInterface.GtkInterface.NewProjectView    as View
import qualified Komposition.UserInterface.GtkInterface.TimelineView      as View
import qualified Komposition.UserInterface.GtkInterface.WelcomeScreenView as View

data Env = Env { cssPath :: FilePath, screen :: Gdk.Screen }

newtype GtkUserInterface m i o a = GtkUserInterface
  (FSM m i o a) deriving (IxFunctor, IxPointed, IxApplicative, IxMonad, MonadFSM, IxMonadTrans)

runGtkUserInterface' :: Monad m => GtkUserInterface m Empty Empty a -> m a
runGtkUserInterface' (GtkUserInterface a) = FSM.runFSM a

-- TODO: Remove this instance and type class (only used inside this
-- module, can just be an internal function)
instance MonadIO m => IxMonadIO (GtkUserInterface m) where
  iliftIO = ilift . liftIO

deriving instance Monad m => Functor (GtkUserInterface m i i)
deriving instance Monad m => Applicative (GtkUserInterface m i i)
deriving instance Monad m => Monad (GtkUserInterface m i i)

data GtkWindow window event = GtkWindow
  { markup       :: GtkWindowMarkup window event
  , widgetState  :: Declarative.SomeState
  , windowEvents :: EventListener event
  , viewEvents   :: EventListener event
  , windowKeyMap :: KeyMap event
  }

asGtkWindow :: GtkWindow window event -> IO Gtk.Window
asGtkWindow w =
  Declarative.someStateWidget (widgetState w) >>= Gtk.unsafeCastTo Gtk.Window

newtype GtkBackgroundProcess a = GtkBackgroundProcess (Async a)

instance Show (GtkBackgroundProcess a) where
  show _ = "<GtkUserInterface background process>"

instance (Member (Reader Env) sig, Carrier sig m, MonadIO m)
  => WindowUserInterface (GtkUserInterface m) where
  type Window (GtkUserInterface m) = GtkWindow
  type WindowMarkup (GtkUserInterface m) = GtkWindowMarkup
  type BackgroundProcess (GtkUserInterface m) = GtkBackgroundProcess ()

  newWindow name markup' keyMap =
    ilift ask >>>= \env ->
      (FSM.new name =<<< irunUI (do
        s <- Declarative.create markup'
        win <- Gtk.unsafeCastTo Gtk.Window =<< Declarative.someStateWidget s
        -- Set up CSS provider
        loadCss env win
        -- Set up event listeners
        windowEvents <- applyKeyMap keyMap =<< subscribeKeyEvents =<< Gtk.toWidget win
        viewEvents <- subscribeToDeclarativeWidget markup' s
        -- And show recursively as this is a new widget tree
        #showAll win
        return (GtkWindow markup' s windowEvents viewEvents keyMap)))

  patchWindow name markup' =
    FSM.get name >>>= \w ->
      FSM.enter name =<<<
        case Declarative.patch (widgetState w) (markup w) markup' of
          Declarative.Modify f -> irunUI $ do
            s' <- f
            unsubscribe (viewEvents w)
            viewEvents' <- subscribeToDeclarativeWidget markup' s'
            return w { markup = markup', widgetState = s', viewEvents = viewEvents' }
          Declarative.Replace create' -> irunUI $ do
            Gtk.widgetDestroy =<< asGtkWindow w
            s' <- create'
            win <- Gtk.unsafeCastTo Gtk.Window =<< Declarative.someStateWidget s'
            viewEvents <- subscribeToDeclarativeWidget markup' s'
            #showAll win
            return (GtkWindow markup' s' (windowEvents w) viewEvents (windowKeyMap w))
          Declarative.Keep -> ireturn w

  destroyWindow name =
    FSM.get name >>>= \w ->
      irunUI (Gtk.widgetDestroy =<< asGtkWindow w) >>> FSM.delete name

  withNewWindow name markup keymap action =
    call $
      newWindow name markup keymap
      >>> action
      >>>= \x ->
        destroyWindow name
        >>> ireturn x

  withNewModalWindow parent name markup keymap action =
    FSM.get parent >>>= \p ->
      call $
        newWindow name markup keymap
        >>> FSM.get name
        >>>= \w -> irunUI (do
          cw <- asGtkWindow w
          pw <- asGtkWindow p
          Gtk.windowSetModal cw True
          Gtk.windowSetTypeHint cw Gdk.WindowTypeHintDialog
          Gtk.windowSetTransientFor cw (Just pw))
        >>> action
        >>>= \x ->
          destroyWindow name
          >>> ireturn x

  nextEvent name =
    FSM.get name >>>= (\w -> iliftIO (raceEvent (windowEvents w) (viewEvents w)))

  nextEventOrTimeout n t = FSM.get n >>>= \w -> iliftIO $ do
    let microseconds = round (fromIntegral (diffTimeToPicoseconds t) / 1000000 :: Double)
    race
      (threadDelay microseconds)
      (raceEvent (windowEvents w) (viewEvents w)) >>= \case
        Left () -> return Nothing
        Right e -> return (Just e)

  runInBackground name action =
    FSM.get name >>>= \w ->
      iliftIO . fmap GtkBackgroundProcess . async $
        (action >>= traverse_ (writeChan (events (windowEvents w))))

  cancelProcess (GtkBackgroundProcess process) =
    iliftIO (cancel process)

  setTransientFor childName parentName =
    FSM.get childName >>>= \child' ->
      FSM.get parentName >>>= \parent -> irunUI $ do
        childWindow <- asGtkWindow child'
        parentWindow <- asGtkWindow parent
        Gtk.windowSetTransientFor childWindow (Just parentWindow)

  prompt n title message okText mode =
    let cancelResponse = fromIntegral (fromEnum Gtk.ResponseTypeCancel)
        acceptResponse = fromIntegral (fromEnum Gtk.ResponseTypeAccept)
        okResponse = fromIntegral (fromEnum Gtk.ResponseTypeOk)
        setUp d content = do
          void (Gtk.dialogAddButton d "Cancel" cancelResponse)
          void (Gtk.dialogAddButton d okText okResponse)
          case mode of
            PromptNumber (min', max', step) -> do
              input <- Gtk.spinButtonNewWithRange min' max' step
              Gtk.boxPackStart content input True True 10
              void (Gtk.onEntryActivate input (Gtk.dialogResponse d okResponse))
              return (Just <$> Gtk.spinButtonGetValue input)
            PromptText -> do
              input <- Gtk.entryNew
              Gtk.boxPackStart content input True True 10
              void (Gtk.onEntryActivate input (Gtk.dialogResponse d okResponse))
              return (Just <$> Gtk.entryGetText input)
        toResponse _ getReturnValue r
          | r `elem` [acceptResponse, okResponse] = getReturnValue
          | otherwise = return Nothing
        tearDown d _ = #destroy d
        classes = HashSet.fromList ["prompt"]
    in inNewModalDialog n ModalDialog { message = Just message, .. }

  chooseFile n mode title defaultDir =
    FSM.get n >>>= \w -> iliftIO $ do
    response <- newEmptyMVar
    runUI $ do

      d <- Gtk.new Gtk.FileChooserNative []
      chooser <- Gtk.toFileChooser d
      void (Gtk.fileChooserSetCurrentFolder chooser defaultDir)
      Gtk.fileChooserSetDoOverwriteConfirmation chooser True
      Gtk.fileChooserSetAction chooser (modeToAction mode)
      Gtk.nativeDialogSetTitle d title
      pw <- asGtkWindow w
      Gtk.nativeDialogSetTransientFor d (Just pw)
      Gtk.nativeDialogSetModal d True
      res <- Gtk.nativeDialogRun d
      case toEnum (fromIntegral res) of
        Gtk.ResponseTypeAccept -> Gtk.fileChooserGetFilename d >>= putMVar response
        Gtk.ResponseTypeCancel -> putMVar response Nothing
        _ -> putMVar response Nothing
      Gtk.nativeDialogDestroy d
    takeMVar response
    where
      modeToAction = \case
        Open File ->  Gtk.FileChooserActionOpen
        Save File -> Gtk.FileChooserActionSave
        Open Directory ->  Gtk.FileChooserActionSelectFolder
        Save Directory -> Gtk.FileChooserActionCreateFolder

  progressBar n title producer =
    FSM.get n >>>= \w -> iliftIO $ do
      result <- newEmptyMVar
      runUI $ do
        pw <- asGtkWindow w
        d <-
          Gtk.new
            Gtk.Dialog
            [ #title := title
            , #transientFor := pw
            , #modal := True
            ]
        content      <- Gtk.dialogGetContentArea d
        contentStyle <- Gtk.widgetGetStyleContext content
        Gtk.styleContextAddClass contentStyle "progress-bar"

        pb <- Gtk.new Gtk.ProgressBar [#showText := True]
        msgLabel <- Gtk.new Gtk.Label []
        let updateProgress = forever $ do
              ProgressUpdate msg fraction <- await
              liftIO . runUI $ do
                Gtk.set pb [#fraction := fraction, #text := printFractionAsPercent fraction ]
                Gtk.set msgLabel [#label := msg]
        #packStart content pb False False 10
        #packStart content msgLabel False False 10
        Gtk.set content [#widthRequest := 300]
        #showAll d

        a <- async $ do
          r <- runSafeT (runEffect (tryP (producer >-> updateProgress)))
          void (tryPutMVar result (Just r))
          runUI (#destroy d)

        void . Gtk.on d #destroy $ do
          cancel a
          void (tryPutMVar result Nothing)

      readMVar result

  beep _ = irunUI Gdk.beep

instance UserInterfaceMarkup GtkWindowMarkup where
  welcomeView = GtkTopWindowMarkup View.welcomeScreenView
  newProjectView = GtkModalMarkup . View.newProjectView
  timelineView = GtkTopWindowMarkup . View.timelineView
  libraryView = GtkModalMarkup . View.libraryView
  importView = GtkModalMarkup . View.importView

newtype GtkMainExitedException =
  GtkMainExitedException String deriving (Typeable, Show)

instance Exception GtkMainExitedException

runGtkUserInterface
  :: (Monad m, Carrier sig m)
  => FilePath
  -> (m () -> IO ())
  -> GtkUserInterface (Eff (ReaderC Env m)) Empty Empty ()
  -> IO ()
runGtkUserInterface cssPath runEffects ui = do
  void $ Gst.init Nothing
  void $ Gtk.init Nothing
  screen  <- maybe (fail "No screen?!") return =<< Gdk.screenGetDefault

  withAsync (runEffects (runReader Env { .. } (runGtkUserInterface' ui)) <* Gtk.mainQuit) $ \result-> do
    Gtk.main
    poll result >>= \case
      Nothing -> throwIO (GtkMainExitedException "gtk's main loop exited unexpectedly")
      Just (Left e) -> throwIO e
      Just (Right ()) -> pass

printFractionAsPercent :: Double -> Text
printFractionAsPercent fraction =
  toS (printf "%.0f%%" (fraction * 100) :: Prelude.String)


loadCss :: Env -> Gtk.Window -> IO ()
loadCss Env { cssPath, screen } window' = do
  cssProviderVar <- newMVar Nothing
  reloadCssProvider cssProviderVar
  void $ window' `Gtk.onWidgetKeyPressEvent` \eventKey -> do
    keyVal <- Gdk.getEventKeyKeyval eventKey
    case keyVal of
      Gdk.KEY_F5 -> reloadCssProvider cssProviderVar
      _          -> return ()
    return False
  where
    cssPriority = fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER
    reloadCssProvider var = void . forkIO $ do
      cssProvider <- runUI $ do
        p <- Gtk.cssProviderNew
        flip catch (\(e :: SomeException) -> print e) $ do
          Gtk.cssProviderLoadFromPath p (Text.pack cssPath)
          Gtk.styleContextAddProviderForScreen screen p cssPriority
        return p
      tryTakeMVar var >>= \case
        Just (Just p) ->
          runUI (Gtk.styleContextRemoveProviderForScreen screen p)
        _ -> return ()
      putMVar var (Just cssProvider)

data ModalDialog ctx t = ModalDialog
  { title      :: Text
  , message    :: Maybe Text
  , classes    :: Declarative.ClassSet
  , setUp      :: Gtk.Dialog -> Gtk.Box -> IO ctx
  , toResponse :: Gtk.Dialog -> ctx -> Int32 -> IO (Maybe t)
  , tearDown   :: Gtk.Dialog -> ctx -> IO ()
  }
inNewModalDialog
  :: (IxMonadIO m, MonadFSM m)
  => HasType n (GtkWindow TopWindow event) r
  => Name n
  -> ModalDialog ctx t
  -> m r r (Maybe t)
inNewModalDialog n ModalDialog {..} = FSM.get n >>>= \parentWindow ->
  iliftIO $ do
    response <- newEmptyMVar
    runUI $ do
      pw <- asGtkWindow parentWindow
      d  <- Gtk.new Gtk.Dialog
                    [#title := title, #transientFor := pw, #modal := True]

      content      <- Gtk.dialogGetContentArea d
      contentStyle <- Gtk.widgetGetStyleContext content
      traverse_ (Gtk.styleContextAddClass contentStyle) (HashSet.toList classes)

      case message of
        Just m -> do
          label <- Gtk.new Gtk.Label []
          Gtk.labelSetLabel label m
          Gtk.boxPackStart content label True True 10
        Nothing -> return ()

      ctx <- setUp d content
      #showAll content
      putMVar response =<< toResponse d ctx =<< #run d
      tearDown d ctx
    takeMVar response
