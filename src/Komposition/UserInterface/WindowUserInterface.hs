{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}
-- | Abstraction for window-based user interfaces, where windows are
-- modelled as type-safe state machines.
module Komposition.UserInterface.WindowUserInterface where

import           Komposition.Prelude  hiding (State)

import           Data.Row.Records
import           Data.Time.Clock
import           Motor.FSM            hiding (Delete)
import           Pipes
import           Pipes.Safe           (SafeT)

import           Komposition.KeyMap
import           Komposition.Progress

data WindowType = TopWindow | Modal

class Show (BackgroundProcess m) => WindowUserInterface m where
  type Window m :: WindowType -> Type -> Type
  type WindowMarkup m :: WindowType -> Type -> Type
  type BackgroundProcess m

  newWindow
    :: Typeable event
    => Name n
    -> WindowMarkup m window event
    -> KeyMap event
    -> m r (Extend n (Window m window event) r) ()

  patchWindow
    :: Typeable event
    => HasType n (Window m window event) r
    => ((n .== Window m window event) .// r) ~ r
    => Name n
    -> WindowMarkup m window event
    -> m r r ()

  setTransientFor
    :: Typeable e1
    => Typeable e2
    => HasType child (Window m Modal e1) r
    => HasType parent (Window m TopWindow e2) r
    => Name child
    -> Name parent
    -> m r r ()

  destroyWindow
    :: Typeable e
    => Name n
    -> Actions m '[ n !- Window m window e] r ()

  withNewWindow
    :: ( r' ~ (n .== Window m window event)
       , Typeable event
       )
    => Name n
    -> WindowMarkup m window event
    -> KeyMap event
    -> m r' r' a
    -> m r r a

  withNewModalWindow
    :: ( HasType parent (Window m window parentEvent) r
       , r' ~ (modal .== Window m Modal event)
       , Typeable event
       )
    => Name parent
    -> Name modal
    -> WindowMarkup m Modal event
    -> KeyMap event
    -> m r' r' a
    -> m r r a

  nextEvent
    :: HasType n (Window m window e) r
    => Typeable e
    => Name n
    -> m r r e

  nextEventOrTimeout
    :: HasType n (Window m window e) r
    => Typeable e
    => Name n
    -> DiffTime
    -> m r r (Maybe e)

  runInBackground
    :: HasType n (Window m window e) r
    => Typeable e
    => Name n
    -> IO (Maybe e)
    -> m r r (BackgroundProcess m)

  cancelProcess
    :: BackgroundProcess m
    -> m r r ()

  beep :: Name n -> m r r ()

  prompt
    :: HasType n (Window m TopWindow event) r
    => Typeable event
    => Name n
    -> Text -- ^ Prompt window title.
    -> Text -- ^ Prompt message.
    -> Text -- ^ Button text for confirming the choice.
    -> PromptMode ret -- ^ Type of prompt, decides the return value type.
    -> m r r (Maybe ret)

  -- TODO: Move these to separate functions or widgets

  chooseFile
    :: HasType n (Window m TopWindow e) r
    => Name n
    -> FileChooserMode
    -> Text -- ^ Dialog window title.
    -> FilePath
    -> m r r (Maybe FilePath)
  progressBar
    :: Exception e
    => HasType n (Window m TopWindow event) r
    => Name n -- ^ Name of parent window
    -> Text -- ^ Progress window title
    -> Producer ProgressUpdate (SafeT IO) a -- ^ Progress updates producer
    -> m r r (Maybe (Either e a))

data FileChooserType
  = File
  | Directory

data FileChooserMode
  = Open FileChooserType
  | Save FileChooserType

data PromptMode ret where
  PromptNumber ::(Double, Double, Double) -> PromptMode Double
  PromptText ::PromptMode Text
