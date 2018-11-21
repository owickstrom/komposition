{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Komposition.UserInterface.GtkInterface.NumberInput
  ( NumberInputProperties (..)
  , numberInput
  , NumberInputEvent (..)
  ) where

import           Komposition.Prelude

import qualified GI.GObject                                          as GI
import qualified GI.Gtk                                              as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.Attributes.Collected
import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.State

import           Komposition.UserInterface.GtkInterface.CustomWidget

data NumberInputProperties n = NumberInputProperties
  { value              :: n
  , range              :: (n, n)
  , numberInputClasses :: ClassSet
  } deriving (Eq, Show)


newtype NumberInputEvent n = NumberInputChanged n

numberInput
  :: (Typeable n, Integral n)
  => NumberInputProperties n
  -> Widget (NumberInputEvent n)
numberInput customData = Widget (CustomWidget {..})
  where
    customWidget = Gtk.SpinButton
    customCreate props = do
      spin <- Gtk.new Gtk.SpinButton []
      adj <- propsToAdjustment props
      Gtk.spinButtonSetAdjustment spin adj
      sc <- Gtk.widgetGetStyleContext spin
      updateClasses sc mempty (numberInputClasses props)
      return (SomeState (StateTreeWidget (StateTreeNode spin sc mempty ())))

    customPatch (SomeState st) old new
      | old == new = CustomKeep
      | otherwise = CustomModify $ \(spin :: Gtk.SpinButton) -> do
        adj <- propsToAdjustment new
        Gtk.spinButtonSetAdjustment spin adj
        updateClasses (stateTreeStyleContext (stateTreeNode st))
                      (numberInputClasses old)
                      (numberInputClasses new)
        return (SomeState st)

    customSubscribe _ (spin :: Gtk.SpinButton) cb = do
      h <- Gtk.on spin
                  #valueChanged
                  (cb . NumberInputChanged . round =<< #getValue spin)
      return (fromCancellation (GI.signalHandlerDisconnect spin h))

propsToAdjustment :: Integral n => NumberInputProperties n -> IO Gtk.Adjustment
propsToAdjustment NumberInputProperties { value, range } =
  Gtk.adjustmentNew (fromIntegral value) (fromIntegral (fst range)) (fromIntegral (snd range)) 1 1 0
