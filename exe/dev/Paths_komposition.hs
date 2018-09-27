{-
  Adapted from src/dev/Paths_movie_monad.hs in Movie Monad.

  Movie Monad
  (C) 2017 David Lettier
  lettier.com

  License: https://raw.githubusercontent.com/lettier/movie-monad/master/LICENSE
-}

{-# LANGUAGE OverloadedStrings #-}

module Paths_komposition where

dataDir :: FilePath
dataDir = "./src/data"

getDataFileName :: FilePath -> IO FilePath
getDataFileName a = do
  putStrLn "You are using a fake Paths_komposition."
  return (dataDir ++ "/" ++ a)
