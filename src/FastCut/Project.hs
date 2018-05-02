{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE RankNTypes            #-}
module FastCut.Project where

import           Data.Text        (Text)

import           FastCut.Focus
import           FastCut.Sequence

data Project = Project { projectName :: Text, topSequence :: Sequence (), focus :: Focus }
  deriving (Eq, Show)

newtype Event = FocusEvent FocusEvent
  deriving (Eq, Show)

update :: Project -> Event -> Project
update project = \case
  FocusEvent focusEvent ->
    case modifyFocus (topSequence project) focusEvent (focus project) of
      Left _       -> project
      Right focus' -> project { focus = focus' }
