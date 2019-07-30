{-# LANGUAGE GADTs               #-}
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
import qualified Data.List.NonEmpty             as NonEmpty
import           Data.Vector                    (Vector)
import qualified GI.GObject                     as GI
import qualified GI.Gtk                         as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.EventSource

data SelectBoxProperties a = SelectBoxProperties
  { selected :: a
  , values   :: NonEmpty a
  } deriving (Eq, Show)


newtype SelectBoxEvent a = SelectBoxChanged a

selectBox
  :: (Typeable a, Eq a)
  => Vector (Attribute Gtk.ComboBoxText (SelectBoxEvent a))
  -> (a -> Text)
  -> SelectBoxProperties a
  -> Widget (SelectBoxEvent a)
selectBox customAttributes format customParams = Widget (CustomWidget {..})
  where
    customWidget = Gtk.ComboBoxText
    customCreate props = do
      combo <- Gtk.new Gtk.ComboBoxText []
      for_ (values props) $ \value ->
        #appendText combo (format value)
      case elemIndex (selected props) (values props) of
        Just i  -> #setActive combo (fromIntegral i)
        Nothing -> pass
      return (combo, ())

    customPatch old new ()
      | old == new = CustomKeep
      | otherwise = CustomModify $ \(combo :: Gtk.ComboBoxText) -> do
        #removeAll combo
        for_ (values new) (#appendText combo . format)

    customSubscribe props () (combo :: Gtk.ComboBoxText) cb = do
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
