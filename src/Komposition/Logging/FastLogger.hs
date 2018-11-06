{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | A 'Log' interpreter that uses fast-logger.
module Komposition.Logging.FastLogger (runFastLoggerLog) where


import           Komposition.Prelude       hiding (Type, list, Reader, ask, runReader)

import qualified Data.Text as Text
import           Control.Effect
import           Control.Effect.Reader
import           Control.Effect.Carrier
import           Control.Effect.Sum
import           Komposition.Logging
import           System.Log.FastLogger

newtype FastLoggerLogIOC m a = FastLoggerLogIOC { runFastLoggerLogIOC :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (MonadIO m, Member (Lift IO) sig, Carrier sig m, Member (Reader LoggerSet) sig) => Carrier (Log :+: sig) (FastLoggerLogIOC m) where
  ret = pure
  eff = handleSum (FastLoggerLogIOC . eff . handleCoercible) $ \case
    WriteLog sev msg k -> k =<< do
      ls <- ask
      liftIO (pushLogStr ls ("[" <> toLogStr (Text.toUpper (show sev)) <> "] " <> toLogStr msg))

runFastLoggerLog :: (Monad m, Carrier sig m, Member (Lift IO) sig) => LoggerSet -> Eff (FastLoggerLogIOC (Eff (ReaderC LoggerSet m))) a -> m a
runFastLoggerLog ls = runReader ls . runFastLoggerLogIOC . interpret
