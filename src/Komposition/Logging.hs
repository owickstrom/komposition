{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The 'Log' effect includes operations for writing logs with
-- various severity levels.
module Komposition.Logging (Log(..), Severity(..), Message, logText, logS, logShow, logLnText, logLnS, logLnShow) where

import           Komposition.Prelude    hiding (Type, list)

import           Control.Effect
import           Control.Effect.Carrier

import           Data.Coerce

data Log (m :: * -> *) k
  = WriteLog Severity Message (() -> k)
  deriving (Functor)

data Severity = Trace | Debug | Info | Warning | Error | Fatal deriving (Eq, Show)

type Message = Text

logText ::
     (Member Log sig, Carrier sig m)
  => Severity
  -> Message
  -> m ()
logText sev msg = send (WriteLog sev msg ret)

logS ::
     (Member Log sig, Carrier sig m, StringConv s Text)
  => Severity
  -> s
  -> m ()
logS sev = logText sev . toS

logShow ::
     (Member Log sig, Carrier sig m, Show s)
  => Severity
  -> s
  -> m ()
logShow sev = logText sev . show

logLnText ::
     (Member Log sig, Carrier sig m)
  => Severity
  -> Text
  -> m ()
logLnText sev = logText sev . (<> "\n")

logLnS ::
     (Member Log sig, Carrier sig m, StringConv s Text)
  => Severity
  -> s
  -> m ()
logLnS sev = logText sev . (<> "\n") . toS

logLnShow ::
     (Member Log sig, Carrier sig m, Show s)
  => Severity
  -> s
  -> m ()
logLnShow sev = logText sev . (<> "\n") . show

instance HFunctor Log where
  hmap _ = coerce
  {-# INLINE hmap #-}

instance Effect Log where
  handle st handler = \case
    WriteLog sev msg k -> WriteLog sev msg (handler . (<$ st) . k)
