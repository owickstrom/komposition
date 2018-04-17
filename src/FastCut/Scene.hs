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

data Scene = Scene { sceneName :: Text, topSequence :: Sequence () }
  deriving (Eq, Show)

data Event = FocusEvent FocusEvent
  deriving (Eq, Show)

data SceneView = SceneView { scene :: Scene , focus :: Focus }
  deriving (Eq, Show)

update :: SceneView -> Event -> SceneView
update view = \case
  FocusEvent focusEvent ->
    case modifyFocus (topSequence (scene view)) (focus view) focusEvent of
      Just focus' -> view { focus = focus' }
      Nothing     -> view
  FocusEvent FocusRight -> view
