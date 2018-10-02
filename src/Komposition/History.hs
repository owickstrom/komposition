module Komposition.History (History, initialise, current, edit, undo, redo) where

import           Komposition.Prelude
import           Komposition.Project

-- | The complete 'History' of a 'Project' editing.
data History = History
    { past    :: [Project] -- ^ All previous versions of the 'Project'.
    , current :: Project   -- ^ The current version of the 'Project'.
    , future  :: [Project] -- ^ All future versions of the 'Project' (after a
                           --   series of 'undo' operations).
    } deriving (Eq, Show, Generic)

-- | Create a 'History' comprising only the current version of the 'Project'.
initialise :: Project -> History
initialise p = History [] p []

-- | Edit the 'Project', recording the previous version in the 'History'.
edit :: (Project -> Project) -> History -> History
edit f (History ps c _) = History (c:ps) (f c) []

-- | Undo the previous 'edit', recording the current version as a possible
-- future in the 'History'. Returns @Nothing@ if there is nothing to undo.
undo :: History -> Maybe History
undo (History []     _ _ ) = Nothing
undo (History (p:ps) c fs) = Just $ History ps p (c:fs)

-- | Redo the previously undone 'edit', recording the current version in the
-- 'History'. Returns @Nothing@ if there is nothing to redo.
redo :: History -> Maybe History
redo (History _  _ []    ) = Nothing
redo (History ps c (f:fs)) = Just $ History (c:ps) f fs
