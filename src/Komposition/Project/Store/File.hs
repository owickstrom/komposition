{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | A 'ProjectStore' interpreter that saves projects as directories.
module Komposition.Project.Store.File
  ( runFileProjectStoreIO
  )
where

import           Komposition.Prelude       hiding (Type, list)

import           Control.Effect
import           Control.Effect.Carrier
import           Control.Effect.Sum
import           Control.Exception.Safe
import           Control.Lens
import           Data.Binary               (Binary)
import qualified Data.Binary               as Binary
import           Data.Time.Clock           (diffTimeToPicoseconds,
                                            picosecondsToDiffTime)
import           Komposition.Composition
import           Komposition.Duration
import           Komposition.History
import           Komposition.Library
import           Komposition.Project
import           Komposition.Project.Store
import           Komposition.VideoSettings
import           Komposition.VideoSpeed
import           System.Directory
import           System.FilePath

instance Binary AssetMetadata
instance Binary VideoAsset
instance Binary AudioAsset
instance Binary TranscodedPath
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
instance Binary AllVideoSettings
instance Binary VideoSpeed
instance Binary Project

instance Binary a => Binary (History a)

-- * File-Based Project Store

projectDataFilePath :: ProjectPath -> FilePath
projectDataFilePath p = p ^. unProjectPath </> "project-history.bin"

writeProject :: ExistingProject -> IO (Either SaveProjectError ())
writeProject existingProject =
  runExceptT
    $          liftIO
                 (writeProjectDataFile
                   (projectDataFilePath (existingProject ^. projectPath))
                   (existingProject ^. projectHistory)
                 )
    `catchAny` (\(e :: SomeException) ->
                 throwError (UnexpectedSaveError (show e))
               )

readProjectDataFile
  :: FilePath -> IO (Either OpenProjectError (History Project))
readProjectDataFile p =
  runExceptT
    $          liftIO (Binary.decodeFile p)
    `catchAny` (\(e :: SomeException) ->
                 throwError (InvalidProjectDataFile p (show e))
               )

writeProjectDataFile :: FilePath -> History Project -> IO ()
writeProjectDataFile = Binary.encodeFile

newtype FileProjectStoreIOC m a = FileProjectStoreIOC { runFileProjectStoreIOC :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance (MonadIO m, Carrier sig m) => Carrier (ProjectStore :+: sig) (FileProjectStoreIOC m) where
  ret = pure
  eff = handleSum (FileProjectStoreIOC . eff . handleCoercible) $ \case
    CreateNewProject targetPath newProject k -> k =<< runExceptT (do
      whenM (liftIO (not . null <$> listDirectory targetPath)) $
        throwError (ProjectDirectoryNotEmpty targetPath)
      liftIO (createDirectoryIfMissing False targetPath)
      let existingProject =  ExistingProject (ProjectPath targetPath) (initialise newProject)
      ExceptT (liftIO (writeProject existingProject))
      return existingProject)

    SaveExistingProject existingProject k -> k =<<
      liftIO (writeProject existingProject)

    OpenExistingProject path' k -> k =<< runExceptT (do
      whenM (liftIO (not <$> doesPathExist path')) $
        throwError (ProjectDirectoryDoesNotExist path')
      let projectPath' = ProjectPath path'
          dataFilePath = projectDataFilePath projectPath'
      whenM (liftIO (not <$> doesPathExist dataFilePath)) $
        throwError (ProjectDataFileDoesNotExist dataFilePath)
      existingHistory <- ExceptT (liftIO (readProjectDataFile dataFilePath))
      return (ExistingProject projectPath' existingHistory))
    GetDefaultProjectsDirectory k -> k =<< liftIO getUserDocumentsDirectory
    GetCacheDirectory k -> k =<< liftIO (getXdgDirectory XdgCache "komposition")

runFileProjectStoreIO
  :: (MonadIO m, Carrier sig m) => Eff (FileProjectStoreIOC m) a -> m a
runFileProjectStoreIO = runFileProjectStoreIOC . interpret
