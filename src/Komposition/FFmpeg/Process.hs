{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Komposition.FFmpeg.Process where

import           Komposition.Prelude        hiding (bracket)
import qualified Prelude

import qualified Data.Text                  as Text
import           Pipes
import           Pipes.Safe                 (MonadSafe, bracket)
import           System.Directory
import qualified System.IO                  as IO
import           System.IO.Error            (isDoesNotExistError)
import           System.Process.Typed

import           Komposition.Duration
import           Komposition.FFmpeg.Command
import           Komposition.Progress
import           Komposition.Timestamp

data RenderError
  = ProcessFailed Text
  deriving (Eq, Show)

instance Exception RenderError

isStreaming :: Command -> Bool
isStreaming cmd =
  case output cmd of
    HttpStreamingOutput{} -> True
    UdpStreamingOutput{}  -> True
    FileOutput{}          -> False

runFFmpegCommand
  :: (MonadSafe m, MonadIO m)
  => (Double -> ProgressUpdate)
  -> Duration
  -> Command
  -> Producer ProgressUpdate m ()
runFFmpegCommand toProgress totalDuration cmd = do
  -- If it's rendering to a file, try removing any existing file first.
  case output cmd of
    FileOutput path -> liftIO $ removeFile path `catch` \case
      e | isDoesNotExistError e -> return ()
        | otherwise             -> throwIO e
    _ -> return ()
  let verbosityArgs = if isStreaming cmd then [] else ["-v", "quiet"]
      allArgs = verbosityArgs <> ["-stats", "-nostdin"] <> map toS (printCommandLineArgs cmd)
      process = proc "ffmpeg" allArgs & setStderr createPipe
  printEscapedFFmpegInvocation allArgs
  bracket (startProcess process) stopProcess $ \p -> do
    liftIO (IO.hSetBuffering (getStderr p) IO.NoBuffering)
    delayIfStreaming
    yield (toProgress 0)
    fromCarriageReturnSplit (getStderr p) >-> yieldLines
    waitForExit p
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

    -- TODO: This is a horrible hack to await the FFmpeg streaming
    -- server's start. If there's some reliable way of parsing the output to
    -- ensure the server is up, that would be preferable.
    delayIfStreaming =
      pass -- if isStreaming cmd then liftIO (threadDelay 500000) else return ()


    waitForExit p = waitExitCode p >>= \case
      ExitSuccess   -> return ()
      ExitFailure e -> throwIO
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

printEscapedFFmpegInvocation :: MonadIO m => [Prelude.String] -> m ()
printEscapedFFmpegInvocation args =
  liftIO (putStrLn (Prelude.unwords ("ffmpeg" : map (\a -> "'" <> a <> "'") args)))
