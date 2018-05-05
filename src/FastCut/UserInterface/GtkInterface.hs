{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
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
{-# LANGUAGE RebindableSyntax           #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | A declarative GTK implementation of the 'UserInterface' protocol.
module FastCut.UserInterface.GtkInterface (run) where

import           Prelude                     hiding (log)

import           Control.Concurrent
import           Control.Lens
import           Control.Monad               (void)
import           Control.Monad.Indexed
import           Control.Monad.Indexed.Trans
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Int                    (Int32)
import           Data.Kind
import           Data.Proxy
import           Data.Row.Records            (Empty, HasType, Modify, Row, type (.!))
import           Data.String
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Data.Time.Clock
import           GHC.OverloadedLabels
import qualified GI.Gdk                      as Gdk
import qualified GI.GLib.Constants           as GLib
import           Motor.FSM                   as FSM

import           Control.Monad.Indexed.IO
import           FastCut.Focus
import           FastCut.Project
import           FastCut.Sequence
import           FastCut.UserInterface
import           GI.Gtk.Declarative          as Gtk

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

newtype GtkInterface m (i :: Row Type) (o :: Row Type) a = GtkInterface
  { runGtkInterface :: FSM m i o a
  } deriving (IxFunctor, IxPointed, IxApplicative, IxMonad, MonadFSM, IxMonadTrans)

type Gui m i o a = (MonadReader Env m, MonadIO m) => GtkInterface m i o a

deriving instance Monad m => Functor (GtkInterface m i i)
deriving instance Monad m => Applicative (GtkInterface m i i)
deriving instance Monad m => Monad (GtkInterface m i i)

data GtkInterfaceState s where
  GtkTimelineMode :: SharedState -> GtkInterfaceState TimelineMode
  GtkLibraryMode :: SharedState -> GtkInterfaceState LibraryMode

sharedState :: GtkInterfaceState s -> SharedState
sharedState = \case
  GtkTimelineMode s -> s
  GtkLibraryMode s -> s

withCurrentObject :: Object -> GtkInterfaceState s -> GtkInterfaceState s
withCurrentObject o = \case
  GtkTimelineMode s -> GtkTimelineMode s { currentObject = o }
  GtkLibraryMode s -> GtkLibraryMode s { currentObject = o }

getState n = FSM.get n

getSharedState n = sharedState `FSM.imap` FSM.get n

runUI :: IO () -> IO ()
runUI f = void (Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT (f *> return False))

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

-- render ::
--      ( HasType n (GtkInterfaceState s) r
--      , Modify n (GtkInterfaceState s) r ~ r
--      , (r .! n) ~ GtkInterfaceState s
--      )
--   => Name n
--   -> Object
--   -> GtkInterface m r r ()
render s@SharedState {..} newObj =
  iliftIO $ do
    runUI (patchBox window currentObject newObj)
    return s { currentObject = newObj }
  where
    patchBox :: Gtk.Window -> Object -> Object -> IO ()
    patchBox w o1 o2 =
      case patch o1 o2 of
        Modify f -> do
          Gtk.containerGetChildren w >>= \case
            [] -> return ()
            (c:_) -> f =<< Gtk.toWidget c
        Replace createNew -> do
          Gtk.containerForall w (Gtk.containerRemove w)
          newWidget <- createNew
          Gtk.containerAdd w newWidget
          Gtk.widgetShowAll w
        Keep -> return ()


widthFromDuration :: (RealFrac d) => d -> Int32
widthFromDuration duration = fromIntegral (ceiling duration :: Int) * 50

focusedClass :: Focused -> Text
focusedClass = \case
  Focused             -> "focused"
  TransitivelyFocused -> "transitively-focused"
  Blurred             -> "blurred"

renderClip' :: Focused -> ClipMetadata -> Object
renderClip' focused metadata =
  container
    Box
    [ classes ["clip", focusedClass focused]
    , #orientation := OrientationHorizontal
    , #widthRequest := widthFromDuration (duration metadata)
    ]
    [ BoxChild False False 0 $
      node Label [#label := clipName metadata]
    ]

renderGap :: Focused -> NominalDiffTime -> Object
renderGap focused duration =
  container Box
  [    classes ["gap", focusedClass focused]
  , #orientation := OrientationHorizontal
  , #widthRequest := widthFromDuration duration
  ]
  [node Label []]

renderClip :: Clip Focused t -> Object
renderClip = \case
  VideoClip focused metadata -> renderClip' focused metadata
  AudioClip focused metadata -> renderClip' focused metadata
  VideoGap  focused duration -> renderGap focused duration
  AudioGap  focused duration -> renderGap focused duration

renderSequence :: Sequence Focused -> Object
renderSequence =
  \case
    Sequence focused sub ->
      container
        Box
        [classes ["sequence", focusedClass focused]]
        (map renderSequence sub)
    Composition focused vs as ->
      container
        Box
        [ #orientation := OrientationVertical
        , classes ["composition", focusedClass focused]
        ]
        [ BoxChild False False 0 $
          container
            Box
            [classes ["video", focusedClass focused]]
            (map renderClip vs)
        , BoxChild False False 0 $
          container
            Box
            [classes ["audio", focusedClass focused]]
            (map renderClip as)
        ]
timelineView project focus =
  container
    Box
    [#orientation := OrientationVertical, classes ["scene"]]
    [ BoxChild True True 0 $ node Label [#label := (project ^. projectName)]
    , BoxChild False False 0 $
      container
        ScrolledWindow
        [ #hscrollbarPolicy := PolicyTypeAutomatic
        , #vscrollbarPolicy := PolicyTypeNever
        ]
        (renderSequence
           (applyFocus
              (project ^. topSequence)
              (focus)))
    ]

selectClipView :: Library -> ClipType -> Int -> Object
selectClipView library clipType idx =
  case clipType of
    Video -> renderClips (library ^. videoClips) idx
    Audio -> renderClips (library ^. audioClips) idx
  where
    renderClips clips _idx =
      container
        ScrolledWindow
        [ #hscrollbarPolicy := PolicyTypeNever
        , #vscrollbarPolicy := PolicyTypeAutomatic
        ]
        (container
           Box
           [#orientation := OrientationVertical]
           (map (BoxChild False False 0 . renderClip) clips))
    renderClip :: Clip a t -> Object
    renderClip =
      \case
        VideoClip _ m -> node Label [#label := clipName m]
        AudioClip _ m -> node Label [#label := clipName m]
        VideoGap _ d -> node Label [#label := "GAP?"]
        AudioGap _ d -> node Label [#label := "GAP?"]

instance (MonadReader Env m, MonadIO m) => UserInterface (GtkInterface m) where
  type State (GtkInterface m) = GtkInterfaceState
  start n project focus =
    ilift ask >>>= \env ->
      iliftIO (initializeWindow env (timelineView project focus))
      >>>= FSM.new n . GtkTimelineMode
  updateTimeline n project focus =
    FSM.get n >>>= \(GtkTimelineMode s) ->
      render s (timelineView project focus) >>>= (FSM.enter n . GtkTimelineMode)
  nextTimelineEvent _n = ireturn OpenLibrary
  enterLibrary n lib clipType idx =
    FSM.get n >>>= \(GtkTimelineMode s) ->
      render s (selectClipView lib clipType idx) >>>=
      (FSM.enter n . GtkLibraryMode)
  nextLibraryEvent _n = ireturn LibraryEscape
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
