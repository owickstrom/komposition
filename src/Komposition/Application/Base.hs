{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds        #-}
module Komposition.Application.Base ((>>), (>>=), Application, module X) where

import           Komposition.Prelude         as X hiding (State, get, (>>),
                                                   (>>=))

import           Control.Effect
import           Control.Monad.Indexed.IO    as X
import           Control.Monad.Indexed.Trans as X
import           Komposition.UserInterface   as X
import           Motor.FSM                   as X hiding (Delete, delete)

import           Komposition.Logging         as X

(>>) :: IxMonad m => m i j a -> m j k b -> m i k b
(>>) = (>>>)

(>>=) :: IxMonad m => m i j a -> (a -> m j k b) -> m i k b
(>>=) = (>>>=)

type Application t m sig
   = ( IxPointed (t m)
     , UserInterface (t m)
     , IxMonadIO (t m)
     , IxMonadTrans t
     , Member Log sig
     , Carrier sig m
     , Monad m
     )
