{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Komposition.UserInterface.StubUserInterface where

import           Komposition.Prelude              hiding (State, evalState, get,
                                                   put, throwError)

import           Control.Effect
import           Control.Effect.Carrier           (Carrier)
import           Control.Effect.Error             (runError, throwError)
import           Control.Effect.State             (StateC, evalState, get, put)
import           Control.Monad.Indexed.Trans
import           Data.Functor.Indexed
import           Data.Row.Records                 (Empty)
import           Data.Typeable                    (Proxy (..))
import           Data.Vector                      (Vector)
import qualified Data.Vector                      as Vector
import           Motor.FSM                        (FSM, IxMonad, MonadFSM,
                                                   runFSM)
import qualified Motor.FSM                        as FSM

import           Komposition.UserInterface
import           Komposition.UserInterface.Dialog
import           Komposition.UserInterface.Help

newtype StubUserInterface m i o a = StubUserInterface (FSM m i o a)
  deriving ( IxFunctor
           , IxPointed
           , IxApplicative
           , IxMonad
           , MonadFSM
           , IxMonadTrans
           )

deriving instance Monad m => Functor (StubUserInterface m i i)
deriving instance Monad m => Applicative (StubUserInterface m i i)
deriving instance Monad m => Monad (StubUserInterface m i i)

runStubUserInterface
  :: (Monad m, Carrier sig m, Effect sig)
  => Vector SomeEvent
  -> StubUserInterface (Eff (StateC StubState (Eff (ErrorC StubError m)))) Empty Empty a
  -> m (Either StubError a)
runStubUserInterface events (StubUserInterface ui) =
  runError (evalState events (runFSM ui))

data SomeEvent where
  SomeEvent :: Typeable mode => Event mode -> SomeEvent

type StubState = Vector SomeEvent

data StubWindow event where
  StubWindow :: Typeable event => StubMarkup event -> StubWindow event

data StubMarkup event where
  StubMarkup :: Typeable event => Proxy event -> StubMarkup event

data StubError
  = NoMoreEvents
  | EventModeMismatch
  deriving (Eq, Show)

stubMarkup :: Typeable event => StubMarkup event
stubMarkup = StubMarkup Proxy

instance (Member (State StubState) sig, Member (Error StubError) sig, Carrier sig m, Monad m)
         => WindowUserInterface (StubUserInterface m) where
  type Window (StubUserInterface m) = StubWindow
  type WindowMarkup (StubUserInterface m) = StubMarkup

  newWindow name markup _ = FSM.new name (StubWindow markup)
  patchWindow _ _ = ireturn ()
  setTransientFor _ _ = ireturn ()
  destroyWindow = FSM.delete
  withNewWindow name markup _ action =
    FSM.call (FSM.new name (StubWindow markup) *>> action <<* FSM.delete name)
  withNewModalWindow _ = withNewWindow
  nextEvent name = do
    (_ :: StubWindow event) <- FSM.get name
    next <- ilift pop
    case next of
      Just (SomeEvent (firstEvent :: Event mode)) ->
        case eqT @event @(Event mode) of
          Just Refl -> ireturn firstEvent
          Nothing   -> ilift (throwError EventModeMismatch)
      Nothing -> ilift (throwError NoMoreEvents)
  nextEventOrTimeout name _ = Just <$> nextEvent name
  runInBackground _ _ = ireturn ()
  beep _ = ireturn ()

  -- TODO: Might need this actually stubbed
  prompt _ _ _ _ _ = ireturn Nothing
  -- TODO: Might need this actually stubbed
  chooseFile _ _ _ _ = ireturn Nothing
  -- TODO: Might need this actually stubbed
  progressBar _ _ _ = ireturn Nothing

  -- TODO: This should be refactored to a separate declarative GTK
  -- widget, rather than being in this type class
  previewStream _ _ _ _ = ireturn Nothing


pop
  :: Monad m
  => Member (State (Vector a)) sig
  => Carrier sig m
  => m (Maybe a)
pop = do
  xs <- get
  if Vector.null xs
    then pure Nothing
    else put (Vector.tail xs) $> Just (Vector.head xs)

instance UserInterfaceMarkup StubMarkup where
  welcomeView = stubMarkup
  newProjectView = const stubMarkup
  timelineView = const stubMarkup
  libraryView = const stubMarkup
  importView = const stubMarkup

instance DialogView StubMarkup where
  dialogView _ = stubMarkup

instance HelpView StubMarkup where
  helpView _ = stubMarkup
