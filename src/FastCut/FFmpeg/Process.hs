{-# LANGUAGE GADTs         #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module FastCut.FFmpeg.Process where

import           FastCut.Prelude
import qualified Prelude

import qualified Data.Text              as Text
import           Pipes
import           System.Directory
import qualified System.IO              as IO
import           System.IO.Error        (isDoesNotExistError)
import           System.Process

import           FastCut.Duration
import           FastCut.FFmpeg.Command
import           FastCut.Progress
import           FastCut.Timestamp

data RenderResult
  = Success
  | ProcessFailed Text

runFFmpegCommand
  :: (Double -> ProgressUpdate)
  -> Duration
  -> Command
  -> Producer ProgressUpdate IO RenderResult
runFFmpegCommand toProgress totalDuration cmd = do
  yield (toProgress 0)
  -- If it's rendering to a file, try removing any existing file first.
  case output cmd of
    FileOutput path -> liftIO $ removeFile path `catch` \case
      e | isDoesNotExistError e -> return ()
        | otherwise             -> throwIO e
    _ -> return ()
  let verbosityArgs = ["-v", "quiet"]
      allArgs = verbosityArgs <> ["-stats", "-nostdin"] <> map toS (printCommandLineArgs cmd)
      process = proc "ffmpeg" allArgs
  liftIO (putStrLn (Prelude.unwords ("ffmpeg" : allArgs)))
  (_, _, Just progressOut, ph) <- liftIO
    (createProcess_ "" process { std_err = CreatePipe })
  liftIO (IO.hSetBuffering progressOut IO.NoBuffering)
  fromCarriageReturnSplit progressOut >-> yieldLines
  waitForExit ph
  where
    yieldLines :: MonadIO m => Pipe Text ProgressUpdate m ()
    yieldLines = forever $ do
      line <- await
      case parseTimestampFromProgress line of
        Just currentDuration -> yield
          (toProgress
            (durationToSeconds currentDuration / durationToSeconds totalDuration)
          )
        Nothing -> return ()
    waitForExit ph = liftIO (waitForProcess ph) >>= \case
      ExitSuccess   -> return Success
      ExitFailure e -> return
        (ProcessFailed ("FFmpeg command failed with exit code: " <> show e))

fromCarriageReturnSplit :: MonadIO m => Handle -> Producer Text m ()
fromCarriageReturnSplit h = go mempty
  where
    go buf = liftIO (IO.hIsEOF h) >>= \case
      True  -> yield buf
      False -> do
        c <- liftIO (IO.hGetChar h)
        if c == '\r'
          then yield buf >> go mempty
          else go (buf <> Text.singleton c)

parseTimestampFromProgress :: Text -> Maybe Duration
parseTimestampFromProgress line = parseTimestamp
  =<< Prelude.lookup "time" (toPairs (splitByWhitespaceOrEquals line))
  where
    splitByWhitespaceOrEquals =
      filter (not . Text.null) . Text.split (`elem` ['\t', ' ', '='])
    toPairs (key : value : rest) = (key, value) : toPairs rest
    toPairs _                    = []
