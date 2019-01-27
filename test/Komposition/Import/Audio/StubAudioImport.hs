{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Komposition.Import.Audio.StubAudioImport where

import           Komposition.Prelude      hiding (bracket, catch)

import           Control.Effect
import           Control.Effect.Carrier
import           Control.Effect.Sum

import           Komposition.Import.Audio

newtype StubAudioImportC m a = StubAudioImportC { runStubAudioImportC :: m a }
  deriving (Functor, Applicative, Monad)

instance (Applicative m, Carrier sig m) => Carrier (AudioImport :+: sig) (StubAudioImportC m) where
  ret = pure
  eff = handleSum (StubAudioImportC . eff . handleCoercible) $ \case
    ImportAudioFile _classification _srcFile _outDir k -> k (pure [])
    IsSupportedAudioFile _ k -> k True

runStubAudioImport :: (Applicative m, Carrier sig m) => Eff (StubAudioImportC m) a -> m a
runStubAudioImport = runStubAudioImportC . interpret
