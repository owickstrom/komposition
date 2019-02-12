{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RankNTypes       #-}
module Komposition.Import.Video where

import           Komposition.Prelude        hiding (catch)

import           Control.Effect
import           Control.Effect.Carrier
import           Data.Coerce
import           Pipes                      (Producer)
import           Pipes.Safe

import           Komposition.Classification
import           Komposition.Duration
import           Komposition.Library
import           Komposition.Progress
import           Komposition.VideoSettings
import           Komposition.VideoSpeed

data VideoImport (m :: * -> *) k
  = Transcode VideoSettings OriginalPath Duration FilePath (Producer ProgressUpdate (SafeT IO) TranscodedPath -> k)
  | GenerateVideoThumbnail OriginalPath FilePath (Maybe FilePath -> k)
  | ImportVideoFile Classification AllVideoSettings VideoSpeed FilePath FilePath (Producer ProgressUpdate (SafeT IO) [VideoAsset] -> k)
  | IsSupportedVideoFile FilePath (Bool -> k)
  deriving (Functor)

instance HFunctor VideoImport where
  hmap _ = coerce
  {-# INLINE hmap #-}

instance Effect VideoImport where
  handle st handler = \case
    Transcode settings original fullLength outDir k ->
      Transcode settings original fullLength outDir (handler . (<$ st) . k)
    GenerateVideoThumbnail srcFile outDir k ->
      GenerateVideoThumbnail srcFile outDir (handler . (<$ st) . k)
    ImportVideoFile classification videoSettings videoSpeed' srcFile outDir k ->
      ImportVideoFile classification videoSettings videoSpeed' srcFile outDir (handler . (<$ st) . k)
    IsSupportedVideoFile path' k ->
      IsSupportedVideoFile path' (handler . (<$ st) . k)

generateVideoThumbnail
  :: (Member VideoImport sig, Carrier sig m)
  => OriginalPath
  -> FilePath
  -> m (Maybe FilePath)
generateVideoThumbnail srcFile outDir =
  send (GenerateVideoThumbnail srcFile outDir ret)

transcode
  :: (Member VideoImport sig, Carrier sig m)
  => VideoSettings
  -> OriginalPath
  -> Duration
  -> FilePath
  -> m (Producer ProgressUpdate (SafeT IO) TranscodedPath)
transcode settings original fullLength outDir =
  send (Transcode settings original fullLength outDir ret)

importVideoFile
  :: (Member VideoImport sig, Carrier sig m)
  => Classification
  -> AllVideoSettings
  -> VideoSpeed
  -> FilePath
  -> FilePath
  -> m (Producer ProgressUpdate (SafeT IO) [VideoAsset])
importVideoFile classification settings videoSettings srcFile outDir = send
  (ImportVideoFile classification settings videoSettings srcFile outDir ret)

isSupportedVideoFile
  :: (Member VideoImport sig, Carrier sig m) => FilePath -> m Bool
isSupportedVideoFile srcFile = send (IsSupportedVideoFile srcFile ret)
