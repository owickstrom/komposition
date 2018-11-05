{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE RankNTypes       #-}
module Komposition.Import.Video where

import           Komposition.Prelude       hiding (catch)

import           Control.Effect
import           Control.Effect.Carrier
import           Data.Coerce

import           Pipes                     (Producer)
import           Pipes.Safe

import           Komposition.Duration
import           Komposition.Library
import           Komposition.Progress
import           Komposition.VideoSettings

data VideoImport (m :: * -> *) k
  = GenerateProxy VideoSettings OriginalPath Duration FilePath (Producer ProgressUpdate (SafeT IO) ProxyPath -> k)
  | GenerateVideoThumbnail OriginalPath FilePath (Maybe FilePath -> k)
  | ImportVideoFile Classification VideoSettings FilePath FilePath (Producer ProgressUpdate (SafeT IO) [VideoAsset] -> k)
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

generateProxy
  :: (Member VideoImport sig, Carrier sig m)
  => VideoSettings
  -> OriginalPath
  -> Duration
  -> FilePath
  -> m (Producer ProgressUpdate (SafeT IO) ProxyPath)
generateProxy settings original fullLength outDir =
  send (GenerateProxy settings original fullLength outDir ret)

importVideoFile
  :: (Member VideoImport sig, Carrier sig m)
  => Classification
  -> VideoSettings
  -> FilePath
  -> FilePath
  -> m (Producer ProgressUpdate (SafeT IO) [VideoAsset])
importVideoFile classification settings srcFile outDir =
  send (ImportVideoFile classification settings srcFile outDir ret)

isSupportedVideoFile
  :: (Member VideoImport sig, Carrier sig m) => FilePath -> m Bool
isSupportedVideoFile srcFile =
  send (IsSupportedVideoFile srcFile ret)

data VideoImportError
  = UnexpectedError FilePath Text
  deriving (Show, Eq)

instance Exception VideoImportError

data Classification = Classified | Unclassified
