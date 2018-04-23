{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE RankNTypes            #-}
module FastCut.Scene where

import           Data.Text        (Text)

import           FastCut.Focus
import           FastCut.Sequence

data Scene = Scene { sceneName :: Text, topSequence :: Sequence (), focus :: Focus }
  deriving (Eq, Show)

data Event = FocusEvent FocusEvent
  deriving (Eq, Show)

update :: Scene -> Event -> Scene
update scene = \case
  FocusEvent focusEvent ->
    case modifyFocus (topSequence scene) focusEvent (focus scene) of
      Left _       -> scene
      Right focus' -> scene { focus = focus' }
