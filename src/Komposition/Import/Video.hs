{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}
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

data VideoImport (m :: * -> *) k
  = Transcode VideoSettings OriginalPath Duration FilePath (Producer ProgressUpdate (SafeT IO) TranscodedPath -> k)
  | GenerateVideoThumbnail OriginalPath FilePath (Maybe FilePath -> k)
  | ImportVideoFile Classification AllVideoSettings FilePath FilePath (Producer ProgressUpdate (SafeT IO) [VideoAsset] -> k)
  | IsSupportedVideoFile FilePath (Bool -> k)
  deriving (Functor)

instance HFunctor VideoImport where
  hmap _ = coerce
  {-# INLINE hmap #-}

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
  -> FilePath
  -> FilePath
  -> m (Producer ProgressUpdate (SafeT IO) [VideoAsset])
importVideoFile classification settings srcFile outDir =
  send (ImportVideoFile classification settings srcFile outDir ret)

isSupportedVideoFile
  :: (Member VideoImport sig, Carrier sig m) => FilePath -> m Bool
isSupportedVideoFile srcFile =
  send (IsSupportedVideoFile srcFile ret)
