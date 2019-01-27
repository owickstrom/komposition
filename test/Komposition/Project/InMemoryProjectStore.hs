{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Komposition.Project.InMemoryProjectStore where

import           Komposition.Prelude       hiding (State, bracket, catch,
                                            evalState, gets, modify, put)

import           Control.Effect
import           Control.Effect.Carrier
import           Control.Effect.State
import           Control.Effect.Sum
import           Control.Lens
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HashMap

import qualified Komposition.History       as History
import           Komposition.Project
import           Komposition.Project.Store

newtype InMemoryProjectStoreC m a =
  InMemoryProjectStoreC { runInMemoryProjectStoreC :: Eff (StateC StoreState m) a }
  deriving (Functor, Applicative, Monad)

type StoreState = HashMap ProjectPath ExistingProject

instance (Monad m, Carrier sig m, Effect sig)
         => Carrier (ProjectStore :+: sig) (InMemoryProjectStoreC m) where
  ret = InMemoryProjectStoreC . ret
  eff = InMemoryProjectStoreC . handleSum (eff . R . handleCoercible) (\case
    CreateNewProject path' project' k -> do
      let ep = ExistingProject (ProjectPath path') (History.initialise project')
      modify (HashMap.insert (ep ^. projectPath) ep)
      runInMemoryProjectStoreC (k (Right ep))
    SaveExistingProject ep k -> do
      modify (HashMap.insert (ep ^. projectPath) ep)
      runInMemoryProjectStoreC (k (Right ()))
    OpenExistingProject path' k -> do
      p <- maybe (Left (ProjectDirectoryDoesNotExist path')) Right
           <$> gets (HashMap.lookup (ProjectPath path'))
      runInMemoryProjectStoreC (k p)
    GetDefaultProjectsDirectory k ->
      runInMemoryProjectStoreC (k "/tmp/stub-project-store-project")
    GetCacheDirectory k -> runInMemoryProjectStoreC (k "/tmp/stub-project-store-cache")
                                                                      )


runInMemoryProjectStore
  :: (Monad m, Carrier sig m, Effect sig)
  => Eff (InMemoryProjectStoreC m) a
  -> m a
runInMemoryProjectStore =
  evalState mempty . runInMemoryProjectStoreC . interpret
