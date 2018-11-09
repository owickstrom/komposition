{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
module Komposition.UserInterface.Dialog where

import           Komposition.Prelude

import           Data.Row.Records
import           Motor.FSM
import           Komposition.UserInterface
import           Komposition.KeyMap

data DialogEvent c
  = DialogChoiceSelected c
  | DialogClosed

data DialogProperties c =
  DialogProperties { dialogTitle :: Text
                   , dialogMessage :: Text
                   , dialogChoices :: [c]
                   }

class DialogView markup where
  dialogView
    :: DialogChoice c
    => Typeable c
    => DialogProperties c
    -> markup (DialogEvent c)

class Enum c => DialogChoice c where
  toButtonLabel :: c -> Text

dialog
  :: ( IxMonad m
     , DialogView (WindowMarkup m)
     , DialogChoice c
     , Typeable c
     , WindowUserInterface m
     , HasType parent (Window m parentEvent) r
     )
  => Name parent
  -> DialogProperties c
  -> m r r (Maybe c)
dialog parent props =
  withNewModalWindow parent #dialog (dialogView props) keyMap
    $    nextEvent #dialog
    >>>= \case
           DialogChoiceSelected c -> ireturn (Just c)
           DialogClosed           -> ireturn Nothing
  where
    keyMap :: KeyMap (DialogEvent c)
    keyMap = KeyMap mempty

-- * Basic dialog types

data Ok = Ok deriving (Eq, Enum)

instance DialogChoice Ok where
  toButtonLabel Ok = "OK"

data Confirmation
  = Yes
  | No
  deriving (Show, Eq, Enum)

instance DialogChoice Confirmation where
  toButtonLabel Yes = "Yes"
  toButtonLabel No = "No"
