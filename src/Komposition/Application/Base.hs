{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
module Komposition.Application.Base
  ( (>>)
  , (>>=)
  , Application
  , module X
  )
where

import           Komposition.Prelude           as X
                                         hiding ( State
                                                , get
                                                , (>>)
                                                , (>>=)
                                                )

import           Control.Monad.Indexed.IO      as X
import           Control.Monad.Indexed.Trans   as X
import           Komposition.UserInterface     as X
import           Komposition.UserInterface.Dialog
import           Motor.FSM                     as X
                                         hiding ( Delete
                                                , delete
                                                )

(>>) :: IxMonad m => m i j a -> m j k b -> m i k b
(>>) = (>>>)

(>>=) :: IxMonad m => m i j a -> (a -> m j k b) -> m i k b
(>>=) = (>>>=)

type Application t m
   = ( IxPointed (t m)
     , IxMonad (t m)
     , IxMonadIO (t m)
     , IxMonadTrans t
     , Monad m
     , WindowUserInterface (t m)
     , DialogView (WindowMarkup (t m))
     )
