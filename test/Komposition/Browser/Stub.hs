{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Komposition.Browser.Stub where

import           Komposition.Prelude    hiding (bracket, catch)

import           Control.Effect
import           Control.Effect.Carrier
import           Control.Effect.Sum

import           Komposition.Browser

newtype StubBrowserC m a = StubBrowserC { runStubBrowserC :: m a }
  deriving (Functor, Applicative, Monad)

instance (Applicative m, Carrier sig m) => Carrier (Browser :+: sig) (StubBrowserC m) where
  ret = pure
  eff = handleSum (StubBrowserC . eff . handleCoercible) $ \case
    OpenBrowser _url k -> k True

runStubBrowser :: (Applicative m, Carrier sig m) => Eff (StubBrowserC m) a -> m a
runStubBrowser = runStubBrowserC . interpret
