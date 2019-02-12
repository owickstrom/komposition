{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Komposition.Render.StubRender where

import           Komposition.Prelude    hiding (bracket, catch)

import           Control.Effect
import           Control.Effect.Carrier
import           Control.Effect.Sum

import           Komposition.Render

newtype StubRenderC m a = StubRenderC { runStubRenderC :: m a }
  deriving (Functor, Applicative, Monad)

instance (Applicative m, Carrier sig m) => Carrier (Render :+: sig) (StubRenderC m) where
  ret = pure
  eff = handleSum (StubRenderC . eff . handleCoercible) $ \case
    RenderComposition _settings _videoSource _output _composition k ->
      k (pure ())
    ExtractFrameToFile _settings _stillFrameMode _videoSource _asset _timeSpan _outDir k ->
      k "/tmp/stub-frame-file.png"

runStubRender :: (Applicative m, Carrier sig m) => Eff (StubRenderC m) a -> m a
runStubRender = runStubRenderC . interpret
