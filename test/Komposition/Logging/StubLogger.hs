{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Komposition.Logging.StubLogger where

import           Komposition.Prelude    hiding (bracket, catch)

import           Control.Effect
import           Control.Effect.Carrier
import           Control.Effect.Sum

import           Komposition.Logging

newtype StubLoggerC m a = StubLoggerC { runStubLoggerC :: m a }
  deriving (Functor, Applicative, Monad)

instance (Applicative m, Carrier sig m) => Carrier (Log :+: sig) (StubLoggerC m) where
  ret = pure
  eff = handleSum (StubLoggerC . eff . handleCoercible) $ \case
    WriteLog _sev _msg k -> k ()

runStubLogger :: (Applicative m, Carrier sig m) => Eff (StubLoggerC m) a -> m a
runStubLogger = runStubLoggerC . interpret
