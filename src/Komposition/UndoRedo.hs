{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Komposition.UndoRedo
  ( Runnable(..)
  , Direction(..)
  , History
  , init
  , current
  , size
  , numUndos
  , numRedos
  , runAndRecord
  , undo
  , redo
  )
where

import           Komposition.Prelude

import           Control.Lens        hiding (uncons)

class Runnable action subject f where
  run :: action 'Forward -> subject -> f (action 'Backward, subject)
  revert :: action 'Backward -> subject -> f (action 'Forward, subject)

data Direction = Forward | Backward

data Directed (dir :: Direction) action where
  ForwardAction :: action -> Directed Forward action
  BackwardAction :: action -> Directed Backward action

deriving instance Eq action => Eq (Directed dir action)
deriving instance Show action => Show (Directed dir action)

type family Flip (d :: Direction) where
  Flip Forward = Backward
  Flip Backward = Forward

data History (action :: Direction -> Type) state = History
  { _current :: state
  , toUndo   :: [action Backward]
  , toRedo   :: [action Forward]
  }

deriving instance
  (Eq state, Eq (action 'Forward), Eq (action 'Backward))
  => Eq (History action state)

deriving instance
  (Show state, Show (action 'Forward), Show (action 'Backward))
  => Show (History action state)

makeLenses ''History

init :: state -> History action state
init current' = History current' mempty mempty

-- TODO: track size in structure instead of counting lists
size :: History action state -> Int
size History { toUndo, toRedo } = length toUndo + length toRedo

numUndos :: History action state -> Int
numUndos History { toUndo } = length toUndo

numRedos :: History action state -> Int
numRedos History { toRedo } = length toRedo

runAndRecord
  :: (Functor f, Runnable action state f)
  => action Forward
  -> History action state
  -> f (History action state)
runAndRecord action history =
  let f (inverted, state') = history
        { _current = state'
        , toUndo  = inverted : toUndo history
        , toRedo  = mempty -- clear the redo stack when we "branch" the
                    -- history
        }
  in f <$> run action (_current history)

undo
  :: (Functor f, Runnable action state f)
  => History action state
  -> Maybe (f (History action state))
undo history = do
  (u, us) <- uncons (toUndo history)
  let f (inverted, state') = history
        { _current = state'
        , toUndo  = us
        , toRedo  = inverted : toRedo history
        }
  pure (f <$> revert u (_current history))

redo
  :: (Functor f, Runnable action state f)
  => History action state
  -> Maybe (f (History action state))
redo history = do
  (r, rs) <- uncons (toRedo history)
  let f (inverted, state') = history
        { _current = state'
        , toRedo  = rs
        , toUndo  = inverted : toUndo history
        }
  pure (f <$> run r (_current history))
