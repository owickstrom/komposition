{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module FastCut.Project.View (render) where

import           Data.Int
import           Data.Text          (Text)
import           Data.Time.Clock    (NominalDiffTime)
import           GI.Gtk             hiding ((:=))

import           FastCut.Focus
import           FastCut.Project    hiding (update)
import           FastCut.Sequence
import           GI.Gtk.Declarative

widthFromDuration :: (RealFrac d) => d -> Int32
widthFromDuration duration = fromIntegral (ceiling duration :: Int) * 50

focusedClass :: Focused -> Text
focusedClass = \case
  Focused             -> "focused"
  TransitivelyFocused -> "transitively-focused"
  Blurred             -> "blurred"

renderClip' :: Focused -> ClipMetadata -> Object
renderClip' focused metadata =
  container
    Box
    [ classes ["clip", focusedClass focused]
    , #orientation := OrientationHorizontal
    , #widthRequest := widthFromDuration (duration metadata)
    ]
    [ BoxChild False False 0 $
      node Label [#label := clipName metadata]
    ]

renderGap :: Focused -> NominalDiffTime -> Object
renderGap focused duration =
  container Box
  [    classes ["gap", focusedClass focused]
  , #orientation := OrientationHorizontal
  , #widthRequest := widthFromDuration duration
  ]
  [node Label []]

renderClip :: Clip Focused t -> Object
renderClip = \case
  VideoClip focused metadata -> renderClip' focused metadata
  AudioClip focused metadata -> renderClip' focused metadata
  VideoGap  focused duration -> renderGap focused duration
  AudioGap  focused duration -> renderGap focused duration

renderSequence :: Sequence Focused -> Object
renderSequence =
  \case
    Sequence focused sub ->
      container
        Box
        [classes ["sequence", focusedClass focused]]
        (map renderSequence sub)
    Composition focused videoClips audioClips ->
      container
        Box
        [ #orientation := OrientationVertical
        , classes ["composition", focusedClass focused]
        ]
        [ BoxChild False False 0 $
          container
            Box
            [classes ["video", focusedClass focused]]
            (map renderClip videoClips)
        , BoxChild False False 0 $
          container
            Box
            [classes ["audio", focusedClass focused]]
            (map renderClip audioClips)
        ]

render :: Project -> Object
render Project {..} =
  container
    Box
    [#orientation := OrientationVertical, classes ["scene"]]
    [ BoxChild True True 0 $ node Label [#label := projectName]
    , BoxChild False False 0 $
      container
        ScrolledWindow
        [ #hscrollbarPolicy := PolicyTypeAutomatic
        , #vscrollbarPolicy := PolicyTypeNever
        ]
        (renderSequence (applyFocus topSequence focus))
    ]
