{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The library view of FastCut's GTK interface.
module FastCut.UserInterface.GtkInterface.LibraryView
  ( libraryView
  ) where

import           Data.Text          (Text)

import           FastCut.Focus      (Focused (..))
import           FastCut.Sequence
import           GI.Gtk.Declarative as Gtk

renderClip :: Clip Focused mt -> BoxChild
renderClip clip =
  BoxChild False False 0 $
  case clip of
    VideoClip focused m ->
      node Label [#label := clipName m, classes ["clip", focusedClass focused]]
    AudioClip focused m ->
      node Label [#label := clipName m, classes ["clip", focusedClass focused]]
  where
    focusedClass :: Focused -> Text
    focusedClass =
      \case
        Focused -> "focused"
        TransitivelyFocused -> "transitively-focused"
        Blurred -> "blurred"

libraryView :: [Clip Focused mt] -> Object
libraryView clips =
  container
    ScrolledWindow
    [ #hscrollbarPolicy := PolicyTypeNever
    , #vscrollbarPolicy := PolicyTypeAutomatic
    ]
    (container
       Box
       [#orientation := OrientationVertical, classes ["library"]]
       [ BoxChild
           False
           False
           0
           (node Label [#label := "Library", classes ["heading"]])
       , BoxChild True True 0 $
         container
           Box
           [#orientation := OrientationVertical, classes ["clips"]]
           (map renderClip clips)
       ])
