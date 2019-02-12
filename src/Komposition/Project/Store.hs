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
  , openExistingProject
  , getDefaultProjectsDirectory
  , getCacheDirectory
  , SaveProjectError(..)
  , OpenProjectError(..)
  ) where

import           Komposition.Prelude    hiding (Type, list)

import           Control.Effect
import           Control.Effect.Carrier

import           Data.Coerce

import           Komposition.Project

data ProjectStore (m :: * -> *) k
  = CreateNewProject FilePath (WithoutHistory Project) (Either SaveProjectError (WithoutHistory ExistingProject) -> k)
  | SaveExistingProject (WithoutHistory ExistingProject) (Either SaveProjectError () -> k)
  | OpenExistingProject FilePath (Either OpenProjectError (WithoutHistory ExistingProject) -> k)
  -- TODO: It's a bit hacky to have these in the ProjectStore
  -- effect. Extract to some kind of user environment effect?
  | GetDefaultProjectsDirectory (FilePath -> k)
  | GetCacheDirectory (FilePath -> k)
  deriving (Functor)

createNewProject ::
     (Member ProjectStore sig, Carrier sig m)
  => FilePath
  -> WithoutHistory Project
  -> m (Either SaveProjectError (WithoutHistory ExistingProject))
createNewProject path project' = send (CreateNewProject path project' ret)

saveExistingProject ::
     (Member ProjectStore sig, Carrier sig m)
  => WithoutHistory ExistingProject
  -> m (Either SaveProjectError ())
saveExistingProject project' = send (SaveExistingProject project' ret)

openExistingProject ::
     (Member ProjectStore sig, Carrier sig m)
  => FilePath
  -> m (Either OpenProjectError (WithoutHistory ExistingProject))
openExistingProject path = send (OpenExistingProject path ret)

getDefaultProjectsDirectory ::
     (Member ProjectStore sig, Carrier sig m)
  => m FilePath
getDefaultProjectsDirectory = send (GetDefaultProjectsDirectory ret)

getCacheDirectory ::
     (Member ProjectStore sig, Carrier sig m)
  => m FilePath
getCacheDirectory = send (GetCacheDirectory ret)

instance HFunctor ProjectStore where
  hmap _ = coerce
  {-# INLINE hmap #-}

instance Effect ProjectStore where
  handle st handler = \case
    CreateNewProject path' project' k -> CreateNewProject path' project' (handler . (<$ st) . k)
    SaveExistingProject project' k -> SaveExistingProject project' (handler . (<$ st) . k)
    OpenExistingProject path' k -> OpenExistingProject path' (handler . (<$ st) . k)
    GetDefaultProjectsDirectory k -> GetDefaultProjectsDirectory (handler . (<$ st) . k)
    GetCacheDirectory k -> GetCacheDirectory (handler . (<$ st) . k)

data SaveProjectError
  = ProjectDirectoryNotEmpty FilePath
  | UnexpectedSaveError Text
  deriving (Eq, Show)

data OpenProjectError
  = ProjectDirectoryDoesNotExist FilePath
  | ProjectDataFileDoesNotExist FilePath
  | InvalidProjectDirectory FilePath
  | InvalidProjectDataFile FilePath Text
  deriving (Eq, Show)
