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

import           Data.Vector                    (Vector)
import qualified GI.GObject                     as GI
import qualified GI.Gtk                         as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.EventSource

data NumberInputProperties n = NumberInputProperties
  { value  :: n
  , range  :: (n, n)
  , step   :: n
  , digits :: Word32
  } deriving (Eq, Show)


newtype NumberInputEvent n = NumberInputChanged n

numberInput
  :: (Typeable n, Eq n, NumberInputNum n)
  => Vector (Attribute Gtk.SpinButton (NumberInputEvent n))
  -> NumberInputProperties n
  -> Widget (NumberInputEvent n)
numberInput customAttributes customParams = Widget (CustomWidget {..})
  where
    customWidget = Gtk.SpinButton
    customCreate props = do
      spin <- Gtk.new Gtk.SpinButton []
      adj <- propsToAdjustment props
      Gtk.spinButtonSetAdjustment spin adj
      Gtk.spinButtonSetDigits spin (digits props)
      return (spin, ())

    customPatch old new ()
      | old == new = CustomKeep
      | otherwise = CustomModify $ \spin -> do
        adj <- propsToAdjustment new
        Gtk.spinButtonSetAdjustment spin adj
        Gtk.spinButtonSetDigits spin (digits new)
        return ()

    customSubscribe _params () (spin :: Gtk.SpinButton) cb = do
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
