{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Video importing interfaces.
module FastCut.Video.Import where

import           FastCut.Prelude

import           FastCut.Library
import           FastCut.MediaType

newtype AutoSplit = AutoSplit Bool deriving (Show, Eq)

data VideoImportError
  = UnexpectedError FilePath Text
  deriving (Show, Eq)

class MonadVideoImporter m where
  importVideoFile ::
       FilePath -> FilePath -> m (Either VideoImportError (Asset Video))
  importVideoFileAutoSplit ::
       FilePath -> FilePath -> m (Either VideoImportError [Asset Video])

instance (Monad m, MonadVideoImporter m) =>
         MonadVideoImporter (ReaderT e m) where
  importVideoFile srcFile outDir =
    lift (importVideoFile srcFile outDir)
  importVideoFileAutoSplit srcFile outDir =
    lift (importVideoFileAutoSplit srcFile outDir)
