-- | Transform a timeline by pasting something from the clipboard.

module Komposition.Composition.Paste where

import           Komposition.Prelude

data PastePosition
  = PasteLeftOf
  | PasteRightOf
  deriving (Show, Eq, Ord, Enum, Bounded)

  -- NOTE: Implemented in terms of the "Insert" module.

-- | Pastes the clipboard 'SomeComposition' into the
-- timeline.

-- paste
--   :: Focus (ToFocusType Timeline)
--   -> SomeComposition a
--   -> PastePosition
--   -> Timeline a
--   -> Maybe (Timeline a)
-- paste focus clipboard position = insert focus insertion insertPos
--   where
--     insertPos = case position of
--       PasteLeftOf  -> LeftOf
--       PasteRightOf -> RightOf
--     insertion = case clipboard of
--       SomeSequence  s -> InsertSequence s
--       SomeParallel  s -> InsertParallel s
--       SomeVideoPart s -> InsertVideoParts [s]
--       SomeAudioPart s -> InsertAudioParts [s]
