{-# LANGUAGE TupleSections              #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
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
module FastCut.UserInterface.GtkInterface
  ( runGtkUserInterface
  )
where

import           FastCut.Prelude                                  hiding (state)
import qualified Prelude

import           Control.Lens
import           Control.Monad                                    (void)
import           Control.Monad.Indexed                            ()
import           Control.Monad.Indexed.Trans
import           Control.Monad.Reader
import qualified Data.GI.Base.Properties                          as GI
import qualified Data.HashSet                                     as HashSet
import           Data.Row.Records                                 (Empty)
import           Data.String
import qualified Data.Text                                        as Text
import qualified GI.Gdk                                           as Gdk
import qualified GI.GLib
import qualified GI.GLib.Constants                                as GLib
import qualified GI.Gst                                           as Gst
import           GI.Gtk                                           (AttrOp (..))
import qualified GI.Gtk                                           as Gtk
import qualified GI.Gtk.Declarative                               as Declarative
import           Motor.FSM                                        hiding ((:=))
import qualified Motor.FSM                                        as FSM
import           Pipes
import           Pipes.Safe                                       (runSafeT,
                                                                   tryP)
import           Text.Printf

import           Control.Monad.Indexed.IO
import           FastCut.Progress
import           FastCut.UserInterface
import           FastCut.UserInterface.GtkInterface.EventListener
import           FastCut.UserInterface.GtkInterface.HelpView
import           FastCut.UserInterface.GtkInterface.ImportView
import           FastCut.UserInterface.GtkInterface.LibraryView
import           FastCut.UserInterface.GtkInterface.TimelineView
import           FastCut.VideoSettings

data Env = Env
  { cssPath :: FilePath
  , screen  :: Gdk.Screen
  }

instance MonadIO m => IxMonadIO (GtkInterface m) where
  iliftIO = ilift . liftIO

newtype GtkInterface m i o a = GtkInterface
  { runGtkInterface :: FSM m i o a
  } deriving (IxFunctor, IxPointed, IxApplicative, IxMonad, MonadFSM, IxMonadTrans)

deriving instance Monad m => Functor (GtkInterface m i i)
deriving instance Monad m => Applicative (GtkInterface m i i)
deriving instance Monad m => Monad (GtkInterface m i i)

data Hierarchy a
  = Top a
  | Child a a

lowest :: Hierarchy a -> a
lowest = \case
  Top a -> a
  Child _ a -> a

data AnyDeclarative where
  AnyDeclarative :: Declarative.Widget e -> AnyDeclarative

type CurrentView mode
   = (Hierarchy AnyDeclarative, EventListener (Event mode))

data GtkInterfaceState mode = GtkInterfaceState
  { allEvents         :: EventListener (Event mode)
  , keyMaps           :: KeyMaps
  , currentViewParent :: Hierarchy Gtk.Window
  , currentView       :: CurrentView mode
  }

runUI :: IO a -> IO a
runUI f = do
  ret <- newEmptyMVar
  void . Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
    f >>= putMVar ret
    return False
  takeMVar ret

unsubscribeView :: GtkInterfaceState a -> IO ()
unsubscribeView state = do
  unsubscribe (allEvents state)
  unsubscribe (snd (currentView state))

initializeWindow :: Typeable mode => Env -> Declarative.Widget (Event mode) -> IO Gtk.Window
initializeWindow Env {cssPath, screen} obj =
  runUI $ do
    window' <- Gtk.windowNew Gtk.WindowTypeToplevel
    Gtk.windowSetTitle window' "FastCut"
    void $ Gtk.onWidgetDestroy window' Gtk.mainQuit
    cssProviderVar <- newMVar Nothing
    reloadCssProvider cssProviderVar
    void $
      window' `Gtk.onWidgetKeyPressEvent` \eventKey -> do
        keyVal <- Gdk.getEventKeyKeyval eventKey
        case keyVal of
          Gdk.KEY_F5 -> reloadCssProvider cssProviderVar
          _          -> return ()
        return False
    windowStyle <- Gtk.widgetGetStyleContext window'
    Gtk.styleContextAddClass windowStyle "fastcut"
    Gtk.containerAdd window' =<< Gtk.toWidget =<< Declarative.create obj
    Gtk.widgetShowAll window'
    return window'
  where
    cssPriority = fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER
    reloadCssProvider var =
      void . forkIO $ do
        cssProvider <-
          runUI $ do
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

getFirstChild :: Gtk.Window -> IO (Maybe Gtk.Widget)
getFirstChild w =
  Gtk.containerGetChildren w >>= \case
    []      -> return Nothing
    (c : _) -> return (Just c)

data NewView mode
  = TopView (Declarative.Widget (Event mode))
  | ModalView (Declarative.Widget (Event mode))

newViewWidget :: NewView mode -> Declarative.Widget (Event mode)
newViewWidget = \case
  TopView w -> w
  ModalView w -> w

render ::
  NewView b
  -> GtkInterfaceState a
  -> IO (Hierarchy Gtk.Window, Hierarchy AnyDeclarative, Gtk.Widget)
render newView state =
  runUI $
  case (currentViewParent state, fst (currentView state), newView) of
    -- We have a top view, and patch it with a new one.
    (Top topWindow, Top (AnyDeclarative oldView), TopView newTopView) ->
      (Top topWindow, Top (AnyDeclarative newTopView), ) <$>
      patchIn topWindow oldView newTopView
    -- We had a modal, but go back to only having a top view.
    (Child topWindow modalWindow, Child (AnyDeclarative oldTopView) _, TopView newTopView) -> do
      #destroy modalWindow
      (Top topWindow, Top (AnyDeclarative newTopView), ) <$>
        patchIn topWindow oldTopView newTopView
    -- We had a modal, and patch it with a new one.
    (Child topWindow modalWindow, Child (AnyDeclarative oldTopView) (AnyDeclarative oldModalView), ModalView newModalView) -> do
      case Declarative.patch oldModalView newModalView of
        Declarative.Modify f -> do
          widget <- Gtk.toWidget modalWindow
          f widget
          return
            ( Child topWindow modalWindow
            , Child (AnyDeclarative oldTopView) (AnyDeclarative newModalView)
            , widget)
        Declarative.Replace createNew -> do
          #destroy modalWindow
          newModal <- createNew
          Gtk.castTo Gtk.Window newModal >>= \case
            Just newModalWindow -> do
              #showAll newModalWindow
              Gtk.set newModalWindow [#transientFor := topWindow, #modal := True]
              return
                ( Child topWindow newModalWindow
                , Child (AnyDeclarative oldTopView) (AnyDeclarative newModalView)
                , newModal)
            Nothing -> fail "ModalView widget was not a window."
        Declarative.Keep -> do
          widget <- Gtk.toWidget modalWindow
          return
            ( Child topWindow modalWindow
            , Child (AnyDeclarative oldTopView) (AnyDeclarative newModalView)
            , widget)
    -- We had a top view, and create a new modal.
    (Top topWindow, Top oldTopView, ModalView newModalView) -> do
      widget <- Declarative.create newModalView
      Gtk.castTo Gtk.Window widget >>= \case
        Just modalWindow -> do
          #showAll modalWindow
          Gtk.set modalWindow [#transientFor := topWindow, #attachedTo := topWindow, #modal := True]
          return
            (Child topWindow modalWindow, Child oldTopView (AnyDeclarative newModalView), widget)
        Nothing -> fail "ModalView widget was not a window."
    _ -> fail "Inconsistent render state."

patchIn :: Gtk.Window -> Declarative.Widget e1 -> Declarative.Widget e2 -> IO Gtk.Widget
patchIn w o1 o2 =
  case Declarative.patch o1 o2 of
    Declarative.Modify f ->
      getFirstChild w >>= \case
        Nothing -> crashOnEmpty
        Just c -> do
          f =<< Gtk.toWidget c
          Gtk.widgetShowAll w
          return c
    Declarative.Replace createNew -> do
      Gtk.containerForall w (Gtk.containerRemove w)
      newWidget <- createNew
      Gtk.containerAdd w newWidget
      Gtk.widgetShowAll w
      return newWidget
    Declarative.Keep -> maybe crashOnEmpty return =<< getFirstChild w
  where
    crashOnEmpty = fail "Cannot render in an empty container."

renderFirst
  :: Typeable a => Declarative.Widget (Event a) -> SMode a -> KeyMaps -> Env -> IO (GtkInterfaceState a)
renderFirst view' mode keyMaps env = do
  w <- initializeWindow env view'
  widget <- Declarative.create view'
  viewEvents <- subscribeToDeclarativeWidget view' widget
  allEvents <-
    subscribeKeyEvents w >>= applyKeyMap (keyMaps mode) >>=
    mergeEvents viewEvents
  pure
    GtkInterfaceState
    {currentViewParent = Top w, currentView = (Top (AnyDeclarative view'), viewEvents), ..}

switchView
  :: NewView b -> SMode b -> GtkInterfaceState a -> IO (GtkInterfaceState b)
switchView newView newMode state = do
  (windows, widgets, widget) <- render newView state
  unsubscribeView state
  viewEvents <- subscribeToDeclarativeWidget (newViewWidget newView) widget
  allEvents <-
    subscribeKeyEvents (lowest (currentViewParent state)) >>=
    applyKeyMap (keyMaps state newMode) >>=
    mergeEvents viewEvents
  pure
    GtkInterfaceState
    { currentViewParent = windows
    , currentView = (widgets, viewEvents)
    , keyMaps = keyMaps state
    , ..
    }

switchView'
  :: (MonadFSM m, IxMonadIO m)
  => Name n
  -> NewView b
  -> SMode b
  -> Actions
       m
       '[(FSM.:=) n (GtkInterfaceState a !--> GtkInterfaceState b)]
       r
       ()
switchView' n view' newMode =
  FSM.get n >>>= \s -> iliftIO (switchView view' newMode s) >>>= FSM.enter n

oneOffWidget :: Typeable mode => Declarative.Widget (Event mode) -> SMode mode -> IO Gtk.Widget
oneOffWidget markup _ = Gtk.toWidget =<< Declarative.create markup

printFractionAsPercent :: Double -> Text
printFractionAsPercent fraction =
  toS (printf "%.0f%%" (fraction * 100) :: Prelude.String)

data ModalDialog ctx t = ModalDialog
  { title      :: Text
  , message    :: Maybe Text
  , classes    :: Declarative.ClassSet
  , setUp      :: Gtk.Dialog -> Gtk.Box -> IO ctx
  , toResponse :: Gtk.Dialog -> ctx -> Int32 -> IO (Maybe t)
  , tearDown   :: Gtk.Dialog -> ctx -> IO ()
  }

inNewModalDialog
  :: (MonadFSM m, IxMonadIO m)
  => Name n
  -> ModalDialog ctx t
  -> Actions m '[(FSM.:=) n (Remain (GtkInterfaceState a))] r (Maybe t)
inNewModalDialog n ModalDialog {..} = FSM.get n >>>= \s -> iliftIO $ do
  response <- newEmptyMVar
  runUI $ do
    d <- Gtk.new Gtk.Dialog
                 [#title := title, #transientFor := lowest (currentViewParent s), #modal := True]

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

instance (MonadReader Env m, MonadIO m) => UserInterface (GtkInterface m) where
  type State (GtkInterface m) = GtkInterfaceState

  start n keyMaps model =
    ilift ask
    >>>= iliftIO . renderFirst (timelineView model) STimelineMode keyMaps
    >>>= FSM.new n

  updateTimeline n model =
    switchView' n (TopView (timelineView model)) STimelineMode

  returnToTimeline n model =
    switchView' n (TopView (timelineView model)) STimelineMode

  enterLibrary n model =
    switchView' n (ModalView (libraryView model)) SLibraryMode

  updateLibrary n model =
    switchView' n (ModalView (libraryView model)) SLibraryMode

  enterImport n form =
    switchView' n (ModalView (importView form)) SImportMode

  updateImport n form =
    switchView' n (ModalView (importView form)) SImportMode

  nextEvent n = FSM.get n >>>= iliftIO . readEvent . allEvents

  beep _ = iliftIO (runUI Gdk.beep)

  dialog n title message choices =
    let setUp d _ =
          forM_ choices $ \choice ->
            void (Gtk.dialogAddButton d (toButtonLabel choice) (fromIntegral (fromEnum choice)))
        toResponse _ _ r
          | r < 0 = return Nothing
          | otherwise = return $ Just $ toEnum $ fromIntegral r
        tearDown d _ = #destroy d
        classes = HashSet.fromList ["dialog"]
    in inNewModalDialog n ModalDialog { message = Just message
                                      , ..
                                      }

  prompt n title message okText mode =
    let cancelResponse = fromIntegral (fromEnum Gtk.ResponseTypeCancel)
        acceptResponse = fromIntegral (fromEnum Gtk.ResponseTypeAccept)
        okResponse = fromIntegral (fromEnum Gtk.ResponseTypeOk)
        setUp d content = do
          void (Gtk.dialogAddButton d "Cancel" cancelResponse)
          void (Gtk.dialogAddButton d okText okResponse)
          case mode of
            NumberPrompt (lower, upper) -> do
              input <- Gtk.spinButtonNewWithRange lower upper 0.1
              Gtk.boxPackStart content input True True 10
              void (Gtk.onEntryActivate input (Gtk.dialogResponse d okResponse))
              return (Just <$> Gtk.spinButtonGetValue input)
            TextPrompt -> do
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
    FSM.get n >>>= \s -> iliftIO $ do
    response <- newEmptyMVar
    runUI $ do
      d <- Gtk.new Gtk.FileChooserNative []
      chooser <- Gtk.toFileChooser d
      void (Gtk.fileChooserSetCurrentFolder chooser defaultDir)
      Gtk.fileChooserSetDoOverwriteConfirmation chooser True
      Gtk.fileChooserSetAction chooser (modeToAction mode)
      Gtk.nativeDialogSetTitle d title
      Gtk.nativeDialogSetTransientFor d (Just (lowest (currentViewParent s)))
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
        Open ->  Gtk.FileChooserActionOpen
        Save -> Gtk.FileChooserActionSave

  progressBar n title producer =
    let setUp d content = do
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

          async $ do
            result <- runSafeT (runEffect (tryP (producer >-> updateProgress)))
            runUI (#destroy d)
            return result

        toResponse d job r = do
          when (r < 0) (#destroy d)
          poll job >>= \case
            Just (Left (SomeException e)) -> do
              print e
              return Nothing
            Just (Right a) ->
              return (Just a)
            Nothing ->
              return Nothing
        tearDown _ _ = return ()
        classes = HashSet.fromList ["progress-bar"]
    in inNewModalDialog n ModalDialog { message = Nothing, .. }

  previewStream n uri streamingProcess videoSettings =
    let setUp d content = do
          playbin <-
            Gst.elementFactoryMake "playbin" Nothing `orFailCreateWith` "playbin"
          playbinBus <- Gst.elementGetBus playbin `orFailCreateWith` "playbin bus"
          void . Gst.busAddWatch playbinBus GI.GLib.PRIORITY_DEFAULT $ \_bus msg -> do
            msgType <- Gst.getMessageType msg
            case msgType of
              [Gst.MessageTypeError] -> do
                (gError, _) <- Gst.messageParseError msg
                gErrorText     <- Gst.gerrorMessage gError
                liftIO . putStrLn $ show gError <> ": " <> gErrorText
              [Gst.MessageTypeStateChanged] -> do
                (oldState, newState, _) <- Gst.messageParseStateChanged msg
                putStrLn ("State changed: " <> show oldState <> " -> " <> show newState :: Text)
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

          let updateProgress = forever $ do
                ProgressUpdate _msg fraction <- await
                print fraction
          ffmpegRenderer <- async $ runSafeT (runEffect (streamingProcess >-> updateProgress))

          void . Gtk.onWidgetRealize content $ do
            #packStart content videoWidget True True 0
            #show videoWidget
            #setSizeRequest
              videoWidget
              (fromIntegral (videoSettings ^. resolution . width))
              (fromIntegral (videoSettings ^. resolution . height))
            void $ Gst.elementSetState playbin Gst.StatePlaying

          void . Gtk.onWidgetDestroy d $ do
            void $ Gst.elementSetState playbin Gst.StateNull
            void . forkIO $ do
              putStrLn ("Killing FFmpeg..." :: Text)
              cancel ffmpegRenderer
              putStrLn ("Killed FFmpeg." :: Text)

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

  help n keymaps =
    let setUp _ content = do
          w <- oneOffWidget (helpView keymaps) STimelineMode
          Gtk.boxPackStart content w True True 0
        toResponse _ () _ = return (Just ())
        tearDown d _ = #destroy d
        classes = HashSet.fromList ["help"]
    in inNewModalDialog n ModalDialog { title = "Help", message = Nothing, .. } >>> ireturn ()

  exit n =
    (FSM.get n >>>= iliftIO . unsubscribeView)
    >>> iliftIO Gtk.mainQuit
    >>> delete n

runGtkUserInterface
  :: FilePath -> GtkInterface (ReaderT Env IO) Empty Empty () -> IO ()
runGtkUserInterface cssPath ui = do
  void $ Gst.init Nothing
  void $ Gtk.init Nothing
  screen <- maybe (fail "No screen?!") return =<< Gdk.screenGetDefault

  appLoop <- async (runReaderT (runFSM (runGtkInterface ui)) Env {..})
  Gtk.main
  cancel appLoop
