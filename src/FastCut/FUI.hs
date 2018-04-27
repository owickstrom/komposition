{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

-- | The "functional user interface" module (FUI) represents a hierarchy of
-- immutable user interface objects, usually constructed in a pure setting, and
-- provides a patching mechanism for performing minimal updates using the
-- underlying imperative GTK library.
module FastCut.FUI where

import           Control.Monad          (forM_)
import           Data.GI.Base.CallStack (HasCallStack)
import           Data.HashSet           (HashSet)
import qualified Data.HashSet           as HashSet
import           Data.Maybe             (fromMaybe)
import           Data.Row.Records       hiding (Label, map)
import           Data.Text              (Text)
import           Data.Typeable
import qualified GI.Gtk                 as Gtk

class Patchable o where
  create :: o -> IO GtkWidget
  patch :: GtkWidget -> o -> o -> IO ()

data Object where
  Object :: (Eq o, Show o, Typeable o, Patchable o) => o -> Object

deriving instance Show Object

instance Eq Object where
  Object (a :: o) == Object (b :: n) =
    case eqT @o @n  of
      Just Refl -> a == b
      Nothing   -> False

-- * Rendering

data GtkWidget where
  GtkWidget :: Gtk.IsWidget w => w -> GtkWidget

withGtkWidget :: GtkWidget -> (forall w . Gtk.IsWidget w => w -> r) -> r
withGtkWidget (GtkWidget w) f = f w

unsafeCastTo
  :: (HasCallStack, Gtk.GObject a)
  => GtkWidget
  -> (Gtk.ManagedPtr a -> a)
  -> IO a
unsafeCastTo (GtkWidget obj) = flip Gtk.unsafeCastTo obj

addClasses :: Gtk.IsWidget w => w -> ClassSet -> IO ()
addClasses widget classes = do
  sc <- Gtk.widgetGetStyleContext widget
  mapM_ (Gtk.styleContextAddClass sc) classes

replaceClasses :: Gtk.IsWidget w => w -> ClassSet -> ClassSet -> IO ()
replaceClasses widget old new = do
  sc <- Gtk.widgetGetStyleContext widget
  mapM_ (Gtk.styleContextRemoveClass sc) (HashSet.difference old new)
  mapM_ (Gtk.styleContextAddClass sc)    (HashSet.difference new old)

setSize :: Gtk.IsWidget w => w -> Maybe Size -> IO ()
setSize widget = \case
  Just size ->
    Gtk.widgetSetSizeRequest widget (fromIntegral (size .! #width)) (fromIntegral (size .! #height))
  Nothing -> return ()

patchAll :: Gtk.IsContainer c => c -> [Object] -> [Object] -> IO ()
patchAll container os' ns' = do
  cs <- Gtk.containerGetChildren container
  sequence_ (go [] cs os' ns')
 where
  -- In case we have a corresponding old and new virtual widget, we patch the
  -- GTK widget.
  go actions (w:ws) (Object (o :: t) : os) (Object (n :: t'):ns) =
    case eqT @t @t' of
      Just Refl ->
        let action = patch (GtkWidget w) o n in go (actions ++ [action]) ws os ns
      Nothing ->
        -- TODO: Replace
        let action = fail "Can't do replace yet." in go (actions ++ [action]) ws os ns
  -- When there are new virtual widgets, create and add them.
  go actions [] [] (Object n:ns) =
    let action = do
          widget <- create n
          withGtkWidget widget (Gtk.containerAdd container)
    in  go (actions ++ [action]) [] [] ns
  -- When a virtual widget has been removed, remove the GTK widget from the
  -- container.
  go actions (w:ws) (_:os) [] =
    let action = Gtk.containerRemove container w
    in  go (actions ++ [action]) ws os []
  -- When there are more old virtual widgets than GTK widgets, we can safely
  -- drop the virtual widgets and go on.
  go actions [] (_:_) ns = go actions [] [] ns
  -- But, when there are stray GTK widgets without corresponding virtual
  -- widgets, something has gone terribly wrong, and we clean that mess up by
  -- removing the GTK widget.
  go actions (w:ws) [] ns =
    let action = Gtk.containerRemove container w
    in  go (actions ++ [action]) ws [] ns
  -- Lastly, when we have gone through all widgets, we return the actions.
  go actions [] [] [] = actions

-- * Objects

data Orientation = Vertical | Horizontal
  deriving (Eq, Show)

type ClassSet = HashSet Text

classes :: [Text] -> ClassSet
classes = HashSet.fromList

type Size = Rec ("width" .== Int .+ "height" .== Int)

data Child props = Child (Rec props) Object

deriving instance Forall props Eq => Eq (Child props)
deriving instance Forall props Show => Show (Child props)

childObject :: Child p -> Object
childObject (Child _ o) = o

-- * Label

type LabelProps = "text" .== Maybe Text .+ "classes" .== ClassSet

defaultLabelProps :: Rec LabelProps
defaultLabelProps = #text .== Nothing .+ #classes .== mempty

newtype Label = Label (Rec LabelProps)
  deriving (Eq, Show)

instance Patchable Label where
  create (Label props) = do
    lbl <- Gtk.labelNew (props .! #text)
    lbl `addClasses` (props .! #classes)
    return (GtkWidget lbl)
  patch widget (Label oldProps) (Label newProps) = do
    lbl <- widget `unsafeCastTo` Gtk.Label
    Gtk.labelSetLabel lbl (fromMaybe mempty (newProps .! #text))
    replaceClasses lbl (oldProps .! #classes) (newProps .! #classes)

label :: Rec LabelProps -> Object
label props = Object (Label props)

-- * Box

type BoxChildProps = "expand" .== Bool
                     .+ "fill" .== Bool
                     .+ "padding" .== Word

defaultBoxChildProps :: Rec BoxChildProps
defaultBoxChildProps =
     #expand .== False
  .+ #fill .== False
  .+ #padding .== 0

type BoxProps = "orientation" .== Orientation
                .+ "classes" .== ClassSet
                .+ "size" .== Maybe Size

defaultBoxProps :: Rec BoxProps
defaultBoxProps =
     #orientation .== Horizontal
  .+ #classes .== mempty
  .+ #size .== Nothing

data Box = Box (Rec BoxProps) [Child BoxChildProps]
  deriving (Eq, Show)

box :: Rec BoxProps -> [Child BoxChildProps] -> Object
box props = Object . Box props

instance Patchable Box where
  create (Box props children) = do
    box' <-
      case props .! #orientation of
        Horizontal -> Gtk.boxNew Gtk.OrientationHorizontal 0
        Vertical   -> Gtk.boxNew Gtk.OrientationVertical 0
    box' `setSize` (props .! #size)
    box' `addClasses` (props .! #classes)
    forM_ children $ \(Child childProps (Object child)) -> do
      childWidget <- create child
      withGtkWidget
        childWidget
        (\w ->
           Gtk.boxPackStart
             box'
             w
             (childProps .! #expand)
             (childProps .! #fill)
             (fromIntegral (childProps .! #padding)))
    return (GtkWidget box')
  patch widget (Box oldProps oldChildren) (Box newProps newChildren) = do
    box' <- widget `unsafeCastTo` Gtk.Box
    box' `setSize` (newProps .! #size)
    replaceClasses box' (oldProps .! #classes) (newProps .! #classes)
    patchAll box'
             (map childObject oldChildren)
             (map childObject newChildren)

-- * ScrollArea

newtype ScrollArea = ScrollArea Object
  deriving (Eq, Show)

scrollArea :: Object -> Object
scrollArea = Object . ScrollArea

instance Patchable ScrollArea where
  create (ScrollArea (Object child)) = do
    sa <- Gtk.scrolledWindowNew Gtk.noAdjustment Gtk.noAdjustment
    Gtk.scrolledWindowSetPolicy sa Gtk.PolicyTypeAutomatic Gtk.PolicyTypeNever
    childWidget <- create child
    withGtkWidget childWidget (Gtk.containerAdd sa)
    return (GtkWidget sa)
  patch widget (ScrollArea oldChild) (ScrollArea newChild) = do
    box' <- widget `unsafeCastTo` Gtk.ScrolledWindow
    viewport <-
      Gtk.unsafeCastTo Gtk.Viewport =<<
      requireSingle =<< Gtk.containerGetChildren box'
    patchAll viewport [oldChild] [newChild]
    where
      requireSingle [w] = return w
      requireSingle _ = fail "Expected a single child widget."
