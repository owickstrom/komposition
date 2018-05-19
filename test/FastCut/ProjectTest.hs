{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module FastCut.ProjectTest where

import           Data.Semigroup   ((<>))
import           Test.Tasty.Hspec

import           FastCut.Sequence

spec_appendAt = it "(TODO)" $ 1 `shouldBe` 1

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
