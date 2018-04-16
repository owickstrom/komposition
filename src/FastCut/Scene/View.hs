module FastCut.Scene.View where

import           FastCut.Focus
import           FastCut.Scene

data Command = FocusLeft | FocusRight

data SceneView = SceneView { scene :: Scene , focus :: Focus }
