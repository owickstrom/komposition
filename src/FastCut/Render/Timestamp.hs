{-# LANGUAGE OverloadedStrings #-}
module FastCut.Render.Timestamp where

import           FastCut.Prelude
import qualified Prelude

import qualified Data.Text        as Text
import           Text.Printf

import           FastCut.Duration

printTimestamp :: Duration -> Text
printTimestamp d =
  let
    sec = durationToSeconds d
    hours, minutes :: Int
    hours   = floor (sec / 3600)
    minutes = floor (sec / 60)
    seconds :: Double
    seconds = sec - (fromIntegral hours * 3600) - (fromIntegral minutes * 60)
  in
    toS (printf "%02d:%02d:%f" hours minutes seconds :: Prelude.String)

parseTimestamp :: Text -> Maybe Duration
parseTimestamp t = case Text.splitOn ":" t of
  [hourStr, minStr, secStr] -> do
    hours <- fromIntegral <$> (readDecimal hourStr :: Maybe Integer)
    mins  <- fromIntegral <$> (readDecimal minStr :: Maybe Integer)
    secs  <- readDouble secStr
    pure (durationFromSeconds (hours * 3600 + mins * 60 + secs))
  _ -> Nothing
