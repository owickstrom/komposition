{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleInstances   #-}
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
  , step               :: n
  , digits             :: Word32
  , numberInputClasses :: ClassSet
  } deriving (Eq, Show)


newtype NumberInputEvent n = NumberInputChanged n

numberInput
  :: (Typeable n, Eq n, NumberInputNum n)
  => NumberInputProperties n
  -> Widget (NumberInputEvent n)
numberInput customData = Widget (CustomWidget {..})
  where
    customWidget = Gtk.SpinButton
    customCreate props = do
      spin <- Gtk.new Gtk.SpinButton []
      adj <- propsToAdjustment props
      Gtk.spinButtonSetAdjustment spin adj
      Gtk.spinButtonSetDigits spin (digits props)
      sc <- Gtk.widgetGetStyleContext spin
      updateClasses sc mempty (numberInputClasses props)
      Gtk.widgetShow spin
      return (SomeState (StateTreeWidget (StateTreeNode spin sc mempty ())))

    customPatch (SomeState st) old new
      | old == new = CustomKeep
      | otherwise = CustomModify $ \(spin :: Gtk.SpinButton) -> do
        adj <- propsToAdjustment new
        Gtk.spinButtonSetAdjustment spin adj
        Gtk.spinButtonSetDigits spin (digits new)
        updateClasses (stateTreeStyleContext (stateTreeNode st))
                      (numberInputClasses old)
                      (numberInputClasses new)
        return (SomeState st)

    customSubscribe _ (spin :: Gtk.SpinButton) cb = do
      h <- Gtk.on spin #valueChanged $
        cb . NumberInputChanged . fromDouble =<< #getValue spin
      return (fromCancellation (GI.signalHandlerDisconnect spin h))

propsToAdjustment :: NumberInputNum n => NumberInputProperties n -> IO Gtk.Adjustment
propsToAdjustment NumberInputProperties { value, range, step } =
  Gtk.adjustmentNew
    (toDouble value)
    (toDouble (fst range))
    (toDouble (snd range))
    (toDouble step)
    0.1
    0

class NumberInputNum n where
  fromDouble :: Double -> n
  toDouble :: n -> Double

  default fromDouble :: Integral n => Double -> n
  fromDouble = round

  default toDouble :: Integral n => n -> Double
  toDouble = fromIntegral

instance NumberInputNum Word
instance NumberInputNum Int
instance NumberInputNum Integer

instance NumberInputNum Double where
  fromDouble = identity
  toDouble = identity
