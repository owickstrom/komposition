module Komposition.Import.HashTest where

import           Komposition.Prelude
import           Test.Tasty.Hspec

import           Komposition.Import.Hash

sha1 :: IO [Char]
sha1 = createSHA1BaseName "test1"

spec_hashing :: Spec
spec_hashing = do
  it "creates a different SHA1 given the same basename" $
    join (shouldNotBe <$> sha1 <*> sha1)
