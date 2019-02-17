{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Komposition.Import.Video.StubVideoImport where

import           Komposition.Prelude      hiding (bracket, catch)

import           Control.Effect
import           Control.Effect.Carrier
import           Control.Effect.Sum

import           Komposition.Import.Video
import           Komposition.Library

newtype StubVideoImportC m a = StubVideoImportC { runStubVideoImportC :: m a }
  deriving (Functor, Applicative, Monad)

instance (Applicative m, Carrier sig m) => Carrier (VideoImport :+: sig) (StubVideoImportC m) where
  ret = pure
  eff = handleSum (StubVideoImportC . eff . handleCoercible) $ \case
    Transcode _settings _original _fullLength _outDir k ->
      k (pure (TranscodedPath "/tmp/stub-video-transcoded.mp4"))
    GenerateVideoThumbnail _srcFile _outDir k ->
      k (pure "/tmp/stub-video-thumbnail.png")
    ImportVideoFile _classification _videoSettings _videoSpeed' _srcFile _outDir k ->
      k (pure [])
    IsSupportedVideoFile _ k -> k True

runStubVideoImport :: (Applicative m, Carrier sig m) => Eff (StubVideoImportC m) a -> m a
runStubVideoImport = runStubVideoImportC . interpret
