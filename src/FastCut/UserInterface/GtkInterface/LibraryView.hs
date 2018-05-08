{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The library view of FastCut's GTK interface.
module FastCut.UserInterface.GtkInterface.LibraryView
  ( libraryView
  ) where

import           Control.Lens

import           FastCut.Project
import           FastCut.Sequence
import           GI.Gtk.Declarative as Gtk

libraryView :: Library -> MediaType -> Int -> Object
libraryView lib clipType idx =
  case clipType of
    Video -> renderClips (lib ^. videoClips) idx
    Audio -> renderClips (lib ^. audioClips) idx
  where
    renderClips clips _idx =
      container
        ScrolledWindow
        [ #hscrollbarPolicy := PolicyTypeNever
        , #vscrollbarPolicy := PolicyTypeAutomatic
        ]
        (container
           Box
           [#orientation := OrientationVertical]
           (map (BoxChild False False 0 . renderClip) clips))
    renderClip :: Clip a t -> Object
    renderClip =
      \case
        VideoClip _ m -> node Label [#label := clipName m]
        AudioClip _ m -> node Label [#label := clipName m]
