{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

-- | Property lists on declarative objects, supporting the underlying
-- attributes from "Data.GI.Base.Attributes".

module GI.Gtk.Declarative.Props
  ( PropPair(..)
  , classes
  ) where

import qualified Data.GI.Base.Attributes as GI
import qualified Data.HashSet            as HashSet
import           Data.Text               (Text)
import           Data.Typeable
import           GHC.TypeLits            (Symbol)
import qualified GI.Gtk                  as Gtk

import           GI.Gtk.Declarative.CSS

data PropPair obj where
  (:=)
    :: (GI.AttrGetC info obj attr value
      , GI.AttrOpAllowed 'GI.AttrConstruct info obj
      , GI.AttrOpAllowed 'GI.AttrSet info obj
      , GI.AttrSetTypeConstraint info value
      , Typeable attr
      , Eq value
      )
    =>  GI.AttrLabelProxy (attr :: Symbol) -> value -> PropPair obj
  Classes
    :: Gtk.IsWidget obj
    => ClassSet
    -> PropPair obj

classes :: Gtk.IsWidget obj => [Text] -> PropPair obj
classes = Classes . HashSet.fromList

instance Eq (PropPair obj) where
  ((_ :: GI.AttrLabelProxy attr1) := v1) == ((_ :: GI.AttrLabelProxy attr2) := v2) =
    case eqT @attr1 @attr2 of
      Just Refl -> v1 == v2
      Nothing   -> False
  Classes c1 == Classes c2 = c1 == c2
  _ == _ = False
