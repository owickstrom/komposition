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
import           Control.Lens
import           Control.Monad                                            (void)
import           Control.Monad.Indexed                                    ()
import           Control.Monad.Indexed.Trans
import qualified Data.GI.Base.Properties                                  as GI
import qualified Data.HashSet                                             as HashSet
import           Data.Row.Records                                         (Empty,
                                                                           HasType)
import           Data.String
import qualified Data.Text                                                as Text
import           Data.Time.Clock                                          (diffTimeToPicoseconds)
import qualified GI.Gdk                                                   as Gdk
import qualified GI.GLib.Constants                                        as GLib
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
import           Komposition.UserInterface.GtkInterface.EventListener
import           Komposition.UserInterface.GtkInterface.GtkWindowMarkup
import           Komposition.VideoSettings

import qualified Komposition.UserInterface.GtkInterface.ImportView        as View
import qualified Komposition.UserInterface.GtkInterface.LibraryView       as View
import qualified Komposition.UserInterface.GtkInterface.TimelineView      as View
import qualified Komposition.UserInterface.GtkInterface.WelcomeScreenView as View

data Env = Env { cssPath :: FilePath, screen :: Gdk.Screen }

newtype GtkUserInterface m i o a = GtkUserInterface
  (FSM m i o a) deriving (IxFunctor, IxPointed, IxApplicative, IxMonad, MonadFSM, IxMonadTrans)

runGtkUserInterface' :: Monad m => GtkUserInterface m Empty Empty a -> m a
runGtkUserInterface' (GtkUserInterface a) = FSM.runFSM a

instance MonadIO m => IxMonadIO (GtkUserInterface m) where
  iliftIO = ilift . liftIO

deriving instance Monad m => Functor (GtkUserInterface m i i)
deriving instance Monad m => Applicative (GtkUserInterface m i i)
deriving instance Monad m => Monad (GtkUserInterface m i i)

data GtkWindow event = GtkWindow
  { markup       :: GtkWindowMarkup event
  , widgetState  :: Declarative.SomeState
  , windowEvents :: EventListener event
  , windowKeyMap :: KeyMap event
  }

asGtkWindow :: GtkWindow event -> IO Gtk.Window
asGtkWindow w =
  Declarative.someStateWidget (widgetState w) >>= Gtk.unsafeCastTo Gtk.Window

instance (Member (Reader Env) sig, Carrier sig m, MonadIO m) => WindowUserInterface (GtkUserInterface m) where
  type Window (GtkUserInterface m) = GtkWindow
  type WindowMarkup (GtkUserInterface m) = GtkWindowMarkup

  newWindow name markup'@(GtkWindowMarkup decl) keyMap =
    ilift ask >>>= \env ->
      (FSM.new name =<<< irunUI (do
        s <- Declarative.create decl
        win <- Gtk.unsafeCastTo Gtk.Window =<< Declarative.someStateWidget s
        -- Set up CSS provider
        loadCss env win
        -- Set up event listeners
        viewEvents <- subscribeToDeclarativeWidget decl s
        keyEvents <- applyKeyMap keyMap =<< subscribeKeyEvents =<< Gtk.toWidget win
        allEvents <- mergeEvents viewEvents keyEvents
        -- And show recursively as this is a new widget tree
        #showAll win
        return (GtkWindow markup' s allEvents keyMap)))

  patchWindow name (GtkWindowMarkup decl) =
    FSM.get name >>>= \w ->
      FSM.enter name =<<<
        case Declarative.patch (widgetState w) (unGtkWindowMarkup (markup w)) decl of
          Declarative.Modify f -> irunUI $ do
            s' <- f
            return w { markup = GtkWindowMarkup decl, widgetState = s' }
          Declarative.Replace create' -> irunUI $ do
            Gtk.widgetDestroy =<< asGtkWindow w
            s' <- create'
            win <- Gtk.unsafeCastTo Gtk.Window =<< Declarative.someStateWidget s'
            viewEvents <- subscribeToDeclarativeWidget decl s'
            keyEvents <- applyKeyMap (windowKeyMap w) =<< subscribeKeyEvents =<< Gtk.toWidget win
            allEvents <- mergeEvents viewEvents keyEvents
            #showAll win
            return (GtkWindow (GtkWindowMarkup decl) s' allEvents (windowKeyMap w))
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
        >>>= \w -> (irunUI $ do
          cw <- asGtkWindow w
          pw <- asGtkWindow p
          Gtk.windowSetTransientFor cw (Just pw))
        >>> action
        >>>= \x ->
          destroyWindow name
          >>> ireturn x

  nextEvent name =
    FSM.get name >>>= (iliftIO . readEvent . windowEvents)

  nextEventOrTimeout n t = FSM.get n >>>= \w -> iliftIO $ do
    let microseconds = round (fromIntegral (diffTimeToPicoseconds t) / 1000000 :: Double)
    race
      (threadDelay microseconds)
      (readEvent (windowEvents w)) >>= \case
        Left () -> return Nothing
        Right e -> return (Just e)

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

  previewStream n uri streamingProcess videoSettings =
    let setUp d content = do
          playbin <-
            Gst.elementFactoryMake "playbin" Nothing `orFailCreateWith` "playbin"
          playbinBus <- Gst.elementGetBus playbin `orFailCreateWith` "playbin bus"
          void . Gst.busAddWatch playbinBus GLib.PRIORITY_DEFAULT $ \_bus msg -> do
            msgType <- Gst.getMessageType msg
            case msgType of
              [Gst.MessageTypeError] -> do
                (gError, _) <- Gst.messageParseError msg
                gErrorText     <- Gst.gerrorMessage gError
                liftIO . putStrLn $ show gError <> ": " <> gErrorText
              [Gst.MessageTypeStateChanged] ->
                -- (oldState, newState, _) <- Gst.messageParseStateChanged msg
                -- putStrLn ("State changed: " <> show oldState <> " -> " <> show newState :: Text)
                return ()
              [Gst.MessageTypeEos] ->
                #destroy d
              _ -> return ()
            return True
          gtkSink <-
            Gst.elementFactoryMake "gtksink" Nothing `orFailCreateWith` "GTK sink"
          GI.setObjectPropertyObject playbin "video-sink" (Just gtkSink)
          GI.setObjectPropertyBool playbin "force-aspect-ratio" True

          videoWidget <-
            GI.getObjectPropertyObject gtkSink "widget" Gtk.Widget `orFailCreateWith`
            "sink widget"
          GI.setObjectPropertyBool playbin "force-aspect-ratio" True
          void $ GI.setObjectPropertyString playbin "uri" (Just uri)

          streamingStarted <- newEmptyMVar

          let updateProgress = do
                -- We await the first progress update so that we know the
                -- streaming has started.
                void await
                liftIO (putMVar streamingStarted ())
                forever (void await)
          ffmpegRenderer <- async $ runSafeT (runEffect (streamingProcess >-> updateProgress))

          void . Gtk.onWidgetRealize content $ do
            #packStart content videoWidget True True 0
            #show videoWidget
            #setSizeRequest
              videoWidget
              (fromIntegral (videoSettings ^. resolution . width))
              (fromIntegral (videoSettings ^. resolution . height))
            takeMVar streamingStarted
            -- Start streaming once the server is ready.
            void $ Gst.elementSetState playbin Gst.StatePlaying

          void . Gtk.onWidgetDestroy d $ do
            void $ Gst.elementSetState playbin Gst.StateNull
            void . async $ cancel ffmpegRenderer

        toResponse _ _ = const (return Nothing)
        tearDown d _ = #destroy d
        classes = HashSet.fromList ["preview"]
        orFailCreateWith :: IO (Maybe t) -> Prelude.String -> IO t
        orFailCreateWith action what =
          action >>=
          maybe
            (Prelude.fail ("Couldn't create GStreamer " <> what <> "."))
            return
    in inNewModalDialog n ModalDialog { title = "Preview", message = Nothing, .. }

  beep _ = irunUI Gdk.beep

instance UserInterfaceMarkup GtkWindowMarkup where
  welcomeView = GtkWindowMarkup View.welcomeScreenView
  timelineView = GtkWindowMarkup . View.timelineView
  libraryView = GtkWindowMarkup . View.libraryView
  importView = GtkWindowMarkup . View.importView

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

  appLoop <- async $ do
    runEffects (runReader Env {..} (runGtkUserInterface' ui))
    Gtk.mainQuit
  Gtk.main
  cancel appLoop

printFractionAsPercent :: Double -> Text
printFractionAsPercent fraction =
  toS (printf "%.0f%%" (fraction * 100) :: Prelude.String)

runUI_ :: IO () -> IO ()
runUI_ ma = void (Gdk.threadsAddIdle GLib.PRIORITY_HIGH (ma *> return False))

runUI :: IO a -> IO a
runUI ma = do
  ret <- newEmptyMVar
  runUI_ (ma >>= putMVar ret)
  takeMVar ret

irunUI :: IxMonadIO m => IO a -> m i i a
irunUI = iliftIO . runUI

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
  => HasType n (GtkWindow event) r
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
