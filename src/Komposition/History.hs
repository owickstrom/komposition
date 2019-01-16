{-# LANGUAGE DeriveGeneric #-}
module Komposition.History (History, initialise, current, edit, edit', undo, redo) where

import           Komposition.Prelude

-- | The complete 'History' of 'a' values.
data History a = History
    { past    :: [a] -- ^ All previous versions of the 'a'.
    , current :: a   -- ^ The current version of the 'a'.
    , future  :: [a] -- ^ All future versions of the 'a' (after a
                           --   series of 'undo' operations).
    } deriving (Eq, Show, Generic)

-- | Create a 'History' comprising only the current version of the 'a'.
initialise :: a -> History a
initialise x = History [] x []

-- | Edit the 'a', recording the previous version in the 'History'.
edit :: (a -> a) -> History a -> History a
edit f = runIdentity . edit' (pure . f)

-- | Edit the 'a', recording the previous version in the 'History', in `f`.
edit' :: Functor f => (a -> f a) -> History a -> f (History a)
edit' f (History ps c _) = f c <&> \c' -> History (c:ps) c' []

-- | Undo the previous 'edit', recording the current version as a possible
-- future in the 'History'. Returns @Nothing@ if there is nothing to undo.
undo :: History a -> Maybe (History a)
undo (History []     _ _ ) = Nothing
undo (History (p:ps) c fs) = Just $ History ps p (c:fs)

-- | Redo the previously undone 'edit', recording the current version in the
-- 'History'. Returns @Nothing@ if there is nothing to redo.
redo :: History a -> Maybe (History a)
redo (History _  _ []    ) = Nothing
redo (History ps c (f:fs)) = Just $ History (c:ps) f fs
