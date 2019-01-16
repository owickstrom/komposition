{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | An effect for rendering parts of the timeline.
module Komposition.Render where

import           Komposition.Prelude            hiding (Type, list)

import           Control.Effect
import           Control.Effect.Carrier
import           Data.Coerce
import           Pipes                          (Producer)
import           Pipes.Safe                     (SafeT)

import           Komposition.Duration
import           Komposition.Library
import           Komposition.MediaType
import           Komposition.Progress
import           Komposition.Render.Composition (Composition, StillFrameMode)
import           Komposition.VideoSettings

data Render (m :: * -> *) k where
  RenderComposition
    :: VideoSettings
    -> Source Video
    -> Output
    -> Composition
    -> (Producer ProgressUpdate (SafeT IO) () -> k)
    -> Render m k

  ExtractFrameToFile
    :: VideoSettings
    -> StillFrameMode
    -> Source Video
    -> Asset Video
    -> TimeSpan
    -> FilePath
    -> (FilePath -> k)
    -> Render m k

deriving instance Functor (Render m)

instance HFunctor Render where
  hmap _ = coerce
  {-# INLINE hmap #-}

instance Effect Render where
  handle st handler = \case
    RenderComposition settings videoSource output composition k ->
      RenderComposition settings videoSource output composition (handler . (<$ st) . k)
    ExtractFrameToFile settings stillFrameMode videoSource asset timeSpan outDir k ->
      ExtractFrameToFile settings stillFrameMode videoSource asset timeSpan outDir (handler . (<$ st) . k)

renderComposition
  :: (Member Render sig, Carrier sig m)
  => VideoSettings
  -> Source Video
  -> Output
  -> Composition
  -> m (Producer ProgressUpdate (SafeT IO) ())
renderComposition settings videoSource output composition =
  send (RenderComposition settings videoSource output composition ret)

extractFrameToFile
  :: (Member Render sig, Carrier sig m)
  => VideoSettings
  -> StillFrameMode
  -> Source Video
  -> Asset Video
  -> TimeSpan
  -> FilePath
  -> m FilePath
extractFrameToFile settings stillFrameMode videoSource asset timeSpan outDir =
  send (ExtractFrameToFile settings stillFrameMode videoSource asset timeSpan outDir ret)

data Source (mt :: MediaType) where
  VideoTranscoded :: Source Video
  VideoProxy :: Source Video
  AudioOriginal :: Source Audio

instance Hashable (Source mt) where
  hashWithSalt s = \case
    VideoTranscoded -> s
    VideoProxy    -> s + 1
    AudioOriginal -> s + 2

type Host = Text
type Port = Word

data Output
  = FileOutput FilePath
  | UdpStreamingOutput Host Port
  | HttpStreamingOutput Host Port
