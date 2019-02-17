{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RankNTypes       #-}
module Komposition.Import.Audio where

import           Komposition.Prelude

import           Control.Effect
import           Control.Effect.Carrier
import           Data.Coerce
import           Pipes                      hiding (Effect)
import           Pipes.Safe

import           Komposition.Classification
import           Komposition.Library
import           Komposition.MediaType
import           Komposition.Progress

data AudioImport (m :: * -> *) k
  = ImportAudioFile
    Classification
     FilePath
     FilePath
     (Producer ProgressUpdate (SafeT IO) [Asset Audio] -> k)
  | IsSupportedAudioFile FilePath (Bool -> k)
  deriving (Functor)

instance HFunctor AudioImport where
  hmap _ = coerce
  {-# INLINE hmap #-}

instance Effect AudioImport where
  handle st handler = \case
    ImportAudioFile classification srcFile outDir k ->
      ImportAudioFile classification srcFile outDir (handler . (<$ st) . k)
    IsSupportedAudioFile path' k ->
      IsSupportedAudioFile path' (handler . (<$ st) . k)

importAudioFile
  :: (Member AudioImport sig, Carrier sig m)
  => Classification
  -> FilePath
  -> FilePath
  -> m (Producer ProgressUpdate (SafeT IO) [Asset Audio])
importAudioFile classification srcFile outDir =
  send (ImportAudioFile classification srcFile outDir ret)

isSupportedAudioFile
  :: (Member AudioImport sig, Carrier sig m) => FilePath -> m Bool
isSupportedAudioFile srcFile =
  send (IsSupportedAudioFile srcFile ret)
