{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE LambdaCase       #-}
-- | The 'ProjectStore' effect includes operations for creating,
-- saving, and opening projects.
module Komposition.Project.Store
  ( ProjectStore(..)
  , createNewProject
  , saveExistingProject
  , SaveProjectError(..)
  , openExistingProject
  , OpenProjectError(..)
  ) where

import           Komposition.Prelude    hiding (Type, list)

import           Control.Effect
import           Control.Effect.Carrier

import           Data.Coerce

import           Komposition.Project

data ProjectStore (m :: * -> *) k
  = CreateNewProject FilePath Project (Either SaveProjectError ExistingProject -> k)
  | SaveExistingProject ExistingProject (Either SaveProjectError () -> k)
  | OpenExistingProject FilePath (Either OpenProjectError ExistingProject -> k)
  deriving (Functor)

createNewProject ::
     (Member ProjectStore sig, Carrier sig m)
  => FilePath
  -> Project
  -> m (Either SaveProjectError ExistingProject)
createNewProject path project = send (CreateNewProject path project ret)

saveExistingProject ::
     (Member ProjectStore sig, Carrier sig m)
  => ExistingProject
  -> m (Either SaveProjectError ())
saveExistingProject project = send (SaveExistingProject project ret)

openExistingProject ::
     (Member ProjectStore sig, Carrier sig m)
  => FilePath
  -> m (Either OpenProjectError ExistingProject)
openExistingProject path = send (OpenExistingProject path ret)

instance HFunctor ProjectStore where
  hmap _ = coerce
  {-# INLINE hmap #-}

instance Effect ProjectStore where
  handle st handler = \case
    CreateNewProject path' project k -> CreateNewProject path' project (handler . (<$ st) . k)
    SaveExistingProject project k -> SaveExistingProject project (handler . (<$ st) . k)
    OpenExistingProject path' k -> OpenExistingProject path' (handler . (<$ st) . k)

data SaveProjectError
  = ProjectDirectoryNotEmpty FilePath
  | UnexpectedSaveError Text
  deriving (Eq, Show)

data OpenProjectError
  = ProjectDirectoryDoesNotExist FilePath
  | ProjectDataFileDoesNotExist FilePath
  | InvalidProjectDirectory FilePath
  | InvalidProjectDataFile FilePath
  deriving (Eq, Show)
