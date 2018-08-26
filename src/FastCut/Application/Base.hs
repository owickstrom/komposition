{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds       #-}
module FastCut.Application.Base ((>>), (>>=), Application, module X) where

import           FastCut.Prelude             as X hiding (State, get, (>>),
                                                   (>>=))

import           Control.Monad.Indexed.IO    as X
import           Control.Monad.Indexed.Trans as X
import           FastCut.UserInterface       as X
import           Motor.FSM                   as X hiding (Delete, delete)

(>>) :: IxMonad m => m i j a -> m j k b -> m i k b
(>>) = (>>>)

(>>=) :: IxMonad m => m i j a -> (a -> m j k b) -> m i k b
(>>=) = (>>>=)

type Application t m
   = ( IxPointed (t m)
     , UserInterface (t m)
     , IxMonadIO (t m)
     , IxMonadTrans t
     , Monad m
     )
