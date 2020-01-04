{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE RankNTypes       #-}
module Komposition.Application.Base ((>>), (>>=), ivoid, Application, module X) where

import           Komposition.Prelude              as X hiding (State, get, (>>),
                                                        (>>=))

import           Control.Effect
import           Control.Monad.Indexed.Trans      as X
import           Komposition.Logging              as X
import           Komposition.UserInterface
import           Komposition.UserInterface.WindowUserInterface
import           Komposition.UserInterface.Dialog
import           Komposition.UserInterface.Help
import           Motor.FSM                        as X hiding (Delete, delete)

(>>) :: IxMonad m => m i j a -> m j k b -> m i k b
(>>) = (>>>)

(>>=) :: IxMonad m => m i j a -> (a -> m j k b) -> m i k b
(>>=) = (>>>=)

ivoid :: IxMonad m => m i i a -> m i i ()
ivoid = (>>> (ireturn ()))

type Application t m sig
   = ( IxPointed (t m)
     , IxMonad (t m)
     , IxMonadTrans t
     , Member Log sig
     , Carrier sig m
     , Monad m
     , WindowUserInterface (t m)
     , UserInterfaceMarkup (WindowMarkup (t m))
     , DialogView (WindowMarkup (t m))
     , HelpView (WindowMarkup (t m))
     )
