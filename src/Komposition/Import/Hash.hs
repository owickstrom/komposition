module Komposition.Import.Hash  where

import           Komposition.Prelude

import           Crypto.Hash as H
import           Crypto.Random as R
import qualified Data.ByteString.Lazy.Char8 as Lazy
import qualified Data.ByteString.Char8      as C
import           System.FilePath

-- | create a hashed basename from source file
createSHA1BaseName :: [Char] -> IO [Char]
createSHA1BaseName filepath = do
  rand <- makeHex
  let name = takeBaseName filepath
      lazyBs = Lazy.pack (name <> rand)
  pure $ hex lazyBs

hex :: Lazy.ByteString -> [Char]
hex bs = show (H.hashlazy bs :: Digest SHA1)

makeHex :: IO [Char]
makeHex = C.unpack <$> R.getRandomBytes 10
