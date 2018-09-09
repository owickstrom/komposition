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

import           Control.Monad                                    (void)
import           Control.Monad.Indexed                            ()
import           Control.Monad.Indexed.Trans
import           Control.Monad.Reader
import qualified Data.HashSet                                     as HashSet
import           Data.Row.Records                                 (Empty)
import           Data.String
import qualified Data.Text                                        as Text
import qualified GI.Gdk                                           as Gdk
import qualified GI.GLib.Constants                                as GLib
import           GI.Gtk                                           (AttrOp (..))
import qualified GI.Gtk                                           as Gtk
import qualified GI.Gtk.Declarative                               as Declarative
import           Motor.FSM                                        hiding ((:=))
import qualified Motor.FSM                                        as FSM
import           Pipes
import           Text.Printf

import           Control.Monad.Indexed.IO
import           FastCut.Progress
import           FastCut.UserInterface
import           FastCut.UserInterface.GtkInterface.EventListener
import           FastCut.UserInterface.GtkInterface.HelpView
import           FastCut.UserInterface.GtkInterface.ImportView
import           FastCut.UserInterface.GtkInterface.LibraryView
import           FastCut.UserInterface.GtkInterface.TimelineView

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
initializeWindow Env { cssPath, screen } obj = do
  w <- newEmptyMVar
  runUI $ do
    window' <- Gtk.windowNew Gtk.WindowTypeToplevel
    Gtk.windowSetTitle window' "FastCut"
    void $ Gtk.onWidgetDestroy window' Gtk.mainQuit

    cssProviderVar <- newMVar Nothing
    reloadCssProvider cssProviderVar

    void $ window' `Gtk.onWidgetKeyPressEvent` \eventKey -> do
      keyVal <- Gdk.getEventKeyKeyval eventKey
      case keyVal of
        Gdk.KEY_F5 ->
          runUI
            $       reloadCssProvider cssProviderVar
            `catch` (\(e :: SomeException) -> print e)
        _ -> return ()
      return False

    windowStyle <- Gtk.widgetGetStyleContext window'
    Gtk.styleContextAddClass windowStyle "fastcut"
    Gtk.widgetShowAll window'
    Gtk.containerAdd window' =<< Gtk.toWidget =<< Declarative.create obj
    Gtk.widgetShowAll window'
    putMVar w window'
  takeMVar w
 where
  cssPriority = fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER
  reloadCssProvider var = do
    cssProvider <- Gtk.cssProviderNew
    Gtk.cssProviderLoadFromPath cssProvider (Text.pack cssPath)
    Gtk.styleContextAddProviderForScreen screen cssProvider cssPriority
    takeMVar var >>= \case
      Just p  -> Gtk.styleContextRemoveProviderForScreen screen p
      Nothing -> return ()
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
renderFirst view mode keyMaps env = do
  w <- initializeWindow env view
  widget <- Declarative.create view
  viewEvents <- subscribeToDeclarativeWidget view widget
  allEvents <-
    subscribeKeyEvents w >>= applyKeyMap (keyMaps mode) >>=
    mergeEvents viewEvents
  pure
    GtkInterfaceState
    {currentViewParent = Top w, currentView = (Top (AnyDeclarative view), viewEvents), ..}

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
switchView' n view newMode =
  FSM.get n >>>= \s -> iliftIO (switchView view newMode s) >>>= FSM.enter n

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

  start n keyMaps project focus =
    ilift ask
    >>>= iliftIO . renderFirst (timelineView project focus) STimelineMode keyMaps
    >>>= FSM.new n

  updateTimeline n project focus =
    switchView' n (TopView (timelineView project focus)) STimelineMode

  returnToTimeline n project focus =
    switchView' n (TopView (timelineView project focus)) STimelineMode

  enterLibrary n mediaType assets idx =
    switchView' n (ModalView (libraryView mediaType assets idx)) SLibraryMode

  updateLibrary n mediaType clips idx =
    switchView' n (ModalView (libraryView mediaType clips idx)) SLibraryMode

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
          let updateProgress = forever $ do
                ProgressUpdate fraction <- await
                liftIO . runUI $
                  Gtk.set pb [#fraction := fraction, #text := printFractionAsPercent fraction]
          #add content pb

          jobResult <- newEmptyMVar
          tid <- forkIO $ do
            result <- Pipes.runEffect (producer >-> updateProgress)
            putMVar jobResult result
            runUI (#destroy d)
          return (jobResult, tid)

        toResponse d (jobResult, tid) r = do
          when (r < 0) (#destroy d)
          result <- tryReadMVar jobResult
          when (isNothing result) (killThread tid)
          return result
        tearDown _ _ = return ()
        classes = HashSet.fromList ["progress-bar"]
    in inNewModalDialog n ModalDialog { message = Nothing, .. }

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
  void $ Gtk.init Nothing
  screen <- maybe (fail "No screen?!") return =<< Gdk.screenGetDefault

  void (forkIO (runReaderT (runFSM (runGtkInterface ui)) Env {..}))
  Gtk.main
