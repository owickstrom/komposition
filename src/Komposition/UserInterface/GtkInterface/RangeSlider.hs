{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Komposition.UserInterface.GtkInterface.RangeSlider where

import           Komposition.Prelude

import qualified GI.GObject                                          as GI
import qualified GI.Gtk                                              as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.Attributes.Collected
import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.State

import           Komposition.UserInterface.GtkInterface.CustomWidget

type CustomState = ()

data RangeSliderProperties = RangeSliderProperties
  { range              :: (Double, Double)
  , rangeSliderClasses :: ClassSet
  } deriving (Eq, Show)


newtype RangeSliderEvent = RangeSliderChanged Double

rangeSlider :: RangeSliderProperties -> Widget RangeSliderEvent
rangeSlider customData = Widget (CustomWidget {..})
  where
    customWidget = Gtk.Scale
    customCreate RangeSliderProperties { range, rangeSliderClasses } = do
      scale <- Gtk.new Gtk.Scale []
      uncurry (#setRange scale) range
      Gtk.scaleSetDrawValue scale False
      sc <- Gtk.widgetGetStyleContext scale
      updateClasses sc mempty rangeSliderClasses
      Gtk.widgetShow scale
      return (SomeState (StateTreeWidget (StateTreeNode scale sc mempty ())))

    customPatch (SomeState st) old new
      | old == new = CustomKeep
      | otherwise = CustomModify $ \(scale :: Gtk.Scale) -> do
        uncurry (#setRange scale) (range new)
        updateClasses (stateTreeStyleContext (stateTreeNode st))
                      (rangeSliderClasses old)
                      (rangeSliderClasses new)
        return (SomeState st)

    customSubscribe _ (scale :: Gtk.Scale) cb = do
      h <- Gtk.on scale
                  #valueChanged
                  (cb . RangeSliderChanged =<< #getValue scale)
      return (fromCancellation (GI.signalHandlerDisconnect scale h))
