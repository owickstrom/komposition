{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Komposition.Browser where

import           Komposition.Prelude    hiding (Type, list)

import           Control.Effect
import           Control.Effect.Carrier
import           Control.Effect.Sum
import           Data.Coerce
import qualified Web.Browser            as Browser

import           Komposition.Project

data Browser (m :: * -> *) k
  = OpenBrowser Text (Bool -> k)
  deriving (Functor)

openBrowser :: (Member Browser sig, Carrier sig m) => Text -> m Bool
openBrowser url = send (OpenBrowser url ret)

instance HFunctor Browser where
  hmap _ = coerce
  {-# INLINE hmap #-}

instance Effect Browser where
  handle st handler = \case
    OpenBrowser url k -> OpenBrowser url (handler . (<$ st) . k)

newtype BrowserIOC m a = BrowserIOC { runBrowserIOC :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (MonadIO m, Carrier sig m) => Carrier (Browser :+: sig) (BrowserIOC m) where
  ret = pure
  eff = handleSum (BrowserIOC . eff . handleCoercible) $ \case
    OpenBrowser url k -> k =<< liftIO (Browser.openBrowser (toS url))

runBrowserIO
  :: (MonadIO m, Carrier sig m) => Eff (BrowserIOC m) a -> m a
runBrowserIO = runBrowserIOC . interpret
