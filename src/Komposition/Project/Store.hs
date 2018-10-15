{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Komposition.Project.Store
  ( createNewProject
  , saveExistingProject
  , SaveProjectError(..)
  , openExistingProject
  , OpenProjectError(..)
  ) where

import           Komposition.Prelude       hiding (Type, list)

import           Control.Lens
import           Data.Binary               (Binary)
import qualified Data.Binary               as Binary
import           Data.Time.Clock           (diffTimeToPicoseconds,
                                            picosecondsToDiffTime)
import           System.Directory
import           System.FilePath

import           Komposition.Composition
import           Komposition.Duration
import           Komposition.History
import           Komposition.Library
import           Komposition.Project
import           Komposition.VideoSettings

instance Binary AssetMetadata
instance Binary VideoAsset
instance Binary AudioAsset
instance Binary ProxyPath
instance Binary OriginalPath

instance Binary Duration where
  get = Duration . picosecondsToDiffTime <$> Binary.get
  put (Duration d) = Binary.put (diffTimeToPicoseconds d)

instance Binary TimeSpan

instance Binary a => Binary (VideoPart a)
instance Binary a => Binary (AudioPart a)
instance Binary a => Binary (Parallel a)
instance Binary a => Binary (Sequence a)
instance Binary a => Binary (Timeline a)
instance Binary Library
instance Binary Resolution
instance Binary VideoSettings
instance Binary Project

instance Binary a => Binary (History a)

data SaveProjectError
  = ProjectDirectoryNotEmpty FilePath
  | UnexpectedSaveError Text
  deriving (Eq, Show)

createNewProject :: FilePath -> Project -> IO (Either SaveProjectError ExistingProject)
createNewProject targetPath newProject = runExceptT $ do
  whenM (liftIO (not . null <$> listDirectory targetPath)) $
    throwError (ProjectDirectoryNotEmpty targetPath)
  liftIO (createDirectoryIfMissing False targetPath)
  let existingProject =  ExistingProject (ProjectPath targetPath) (initialise newProject)
  writeProject existingProject
    `catchE` (\(e :: SomeException) -> throwError (UnexpectedSaveError (show e)))
  return existingProject

saveExistingProject :: ExistingProject -> IO (Either SaveProjectError ())
saveExistingProject existingProject = runExceptT $
  writeProject existingProject
    `catchE` (\(e :: SomeException) -> throwError (UnexpectedSaveError (show e)))

data OpenProjectError
  = ProjectDirectoryDoesNotExist FilePath
  | ProjectDataFileDoesNotExist FilePath
  | InvalidProjectDirectory FilePath
  | InvalidProjectDataFile FilePath
  deriving (Eq, Show)

openExistingProject :: FilePath -> IO (Either OpenProjectError ExistingProject)
openExistingProject path' = runExceptT $ do
  whenM (liftIO (not <$> doesPathExist path')) $
    throwError (ProjectDirectoryDoesNotExist path')
  let projectPath' = (ProjectPath path')
      dataFilePath = projectDataFilePath projectPath'
  whenM (liftIO (not <$> doesPathExist dataFilePath)) $
    throwError (ProjectDataFileDoesNotExist dataFilePath)
  existingHistory <- liftIO (readProjectDataFile dataFilePath)
    `catchE` (\(e :: SomeException) -> do
      putStrLn ("Invalid project data file: " <> show e)
      throwError (InvalidProjectDataFile dataFilePath))
  return (ExistingProject projectPath' existingHistory)

projectDataFilePath :: ProjectPath -> FilePath
projectDataFilePath p =
  p ^. unProjectPath </> "project-history.bin"

writeProject :: ExistingProject -> ExceptT e IO ()
writeProject existingProject =
  liftIO $
    writeProjectDataFile
      (projectDataFilePath (existingProject ^. projectPath))
      (existingProject ^. projectHistory)

readProjectDataFile :: FilePath -> IO (History Project)
readProjectDataFile = Binary.decodeFile

writeProjectDataFile :: FilePath -> History Project -> IO ()
writeProjectDataFile = Binary.encodeFile
