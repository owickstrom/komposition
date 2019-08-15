{-# LANGUAGE PolyKinds #-}
module Komposition.UserInterface.GtkInterface.Threading where

import           Komposition.Prelude

import           Control.Monad.Indexed    ()
import           Control.Monad.Indexed.IO
import qualified GI.Gdk                   as Gdk
import qualified GI.GLib.Constants        as GLib

runUI_ :: IO () -> IO ()
runUI_ ma = void (Gdk.threadsAddIdle GLib.PRIORITY_HIGH (ma *> return False))

runUI :: IO a -> IO a
runUI ma = do
  ret <- newEmptyMVar
  runUI_ (ma >>= putMVar ret)
  takeMVar ret

irunUI :: IxMonadIO m => IO a -> m i i a
irunUI = iliftIO . runUI
