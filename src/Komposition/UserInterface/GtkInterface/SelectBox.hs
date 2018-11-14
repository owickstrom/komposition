{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Komposition.UserInterface.GtkInterface.SelectBox
  ( SelectBoxProperties (..)
  , selectBox
  , SelectBoxEvent (..)
  ) where

import           Komposition.Prelude

import           Control.Lens
import qualified Data.List.NonEmpty                                  as NonEmpty
import qualified GI.GObject                                          as GI
import qualified GI.Gtk                                              as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.Attributes.Collected
import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.State

import           Komposition.UserInterface.GtkInterface.CustomWidget

data SelectBoxProperties a = SelectBoxProperties
  { selected         :: a
  , values           :: NonEmpty a
  , selectBoxClasses :: ClassSet
  } deriving (Eq, Show)


newtype SelectBoxEvent a = SelectBoxChanged a

selectBox
  :: (Typeable a, Eq a)
  => (a -> Text)
  -> SelectBoxProperties a
  -> Widget (SelectBoxEvent a)
selectBox format customData = Widget (CustomWidget {..})
  where
    customWidget = Gtk.ComboBoxText
    customCreate props = do
      combo <- Gtk.new Gtk.ComboBoxText []
      for_ (values props) $ \value ->
        #appendText combo (format value)
      case elemIndex (selected props) (values props) of
        Just i  -> #setActive combo (fromIntegral i)
        Nothing -> pass
      sc <- Gtk.widgetGetStyleContext combo
      updateClasses sc mempty (selectBoxClasses props)
      return (SomeState (StateTreeWidget (StateTreeNode combo sc mempty ())))

    customPatch (SomeState st) old new
      | old == new = CustomKeep
      | otherwise = CustomModify $ \(combo :: Gtk.ComboBoxText) -> do
        #removeAll combo
        for_ (values new) $ \value ->
          #appendText combo (format value)
        updateClasses (stateTreeStyleContext (stateTreeNode st))
                      (selectBoxClasses old)
                      (selectBoxClasses new)
        return (SomeState st)

    customSubscribe props (combo :: Gtk.ComboBoxText) cb = do
      h <- Gtk.on combo
                  #changed
                  (cb
                   . SelectBoxChanged
                   . (values props NonEmpty.!!)
                   . fromIntegral
                   =<< #getActive combo)
      return (fromCancellation (GI.signalHandlerDisconnect combo h))

elemIndex :: (FoldableWithIndex i f, Eq a) => a -> f a -> Maybe i
elemIndex x xs =
  xs & ifind (const (== x)) & fmap fst
