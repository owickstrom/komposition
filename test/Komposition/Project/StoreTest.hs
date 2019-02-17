{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Komposition.Project.StoreTest where

import           Komposition.Prelude
import qualified Prelude

import           Control.Effect
import           Control.Lens
import           Hedgehog                       hiding (Parallel)
import           System.IO.Temp

import           Komposition.Project
import qualified Komposition.Project.Generators as Gen
import           Komposition.Project.Store
import           Komposition.Project.Store.File

hprop_createAndOpenNewProject =
  withTests 100 $ property $ do
    newProject <- forAll Gen.project
    tmpRoot <- liftIO getCanonicalTemporaryDirectory
    path <- liftIO $ createTempDirectory tmpRoot "project-store-test"
    existing <- create path newProject
    loaded <- open path
    existing === loaded

hprop_createAndSaveNewProject =
  withTests 100 $ property $ do
    (newProject, modifiedProject) <- forAll ((,) <$> Gen.project <*> Gen.project)
    tmpRoot <- liftIO getCanonicalTemporaryDirectory
    path <- liftIO $ createTempDirectory tmpRoot "project-store-test"
    initialExisting <- create path newProject
    let existingModified = initialExisting & project .~ modifiedProject
    _ <- save existingModified
    loaded <- open path
    loaded === existingModified

runStore = liftIO . runM . runFileProjectStoreIO

create path newProject =
  runStore (createNewProject path newProject) >>= \case
    Left err -> footnoteShow err >> failure
    Right existing -> return existing

open path =
  runStore (openExistingProject path) >>= \case
    Left err -> footnoteShow err >> failure
    Right loaded -> return loaded

save existing =
  runStore (saveExistingProject existing) >>= \case
    Left err -> footnoteShow err >> failure
    Right () -> return ()

{-# ANN module ("HLint: ignore Use camelCase" :: Prelude.String) #-}
