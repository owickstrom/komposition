module FastCut.UserInterface.GtkInterface.View where

import           FastCut.Prelude

import           GI.Gtk.Declarative                               as Gtk

import           FastCut.UserInterface
import           FastCut.UserInterface.GtkInterface.EventListener

data View mode = View
  { markup     :: Markup
  , viewEvents :: EventListener (Event mode)
  }

viewWithEvents :: (Chan (Event mode) -> Markup) -> IO (View mode)
viewWithEvents renderView = do
  es <- newChan
  pure
    View
    { markup = renderView es
    , viewEvents = EventListener es (return ())
    }