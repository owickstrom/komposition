{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module FastCut.Scene.View (renderScene) where

import           Data.Function    ((&))
import           Data.Int
import           Data.Text        (Text)
import           Data.Time.Clock  (NominalDiffTime)
import           GI.Gtk           hiding ((:=))

import           FastCut.Focus
import           FastCut.FUI
import           FastCut.Scene    hiding (update)
import           FastCut.Sequence

widthFromDuration :: (RealFrac d) => d -> Int32
widthFromDuration duration = fromIntegral (ceiling duration :: Int) * 50

focusedClass :: Focused -> Text
focusedClass = \case
  Focused             -> "focused"
  TransitivelyFocused -> "transitively-focused"
  Blurred             -> "blurred"

renderClip' :: Focused -> ClipMetadata -> Object
renderClip' focused metadata =
  container Box [classes ["clip", focusedClass focused]
           , #orientation := OrientationHorizontal
           , #widthRequest := widthFromDuration (duration metadata)
           ]
  [ -- Child defaultBoxChildProps $
    node Label [#label := clipName metadata, classes ["clip"]]
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
      container Box
        [ classes ["sequence", focusedClass focused]
        ]
        (map renderSequence sub)
    Composition focused videoClips audioClips ->
      container Box
          [ #orientation := OrientationVertical
          , classes ["composition", focusedClass focused]
          ]
        [ -- Child defaultBoxChildProps $
          container Box
            [classes ["video", focusedClass focused]]
            (map renderClip videoClips)
        , -- Child defaultBoxChildProps $
          container Box
            [classes ["audio", focusedClass focused]]
            (map renderClip audioClips)
        ]

renderScene :: Scene -> Object
renderScene Scene {..} =
  container Box [ #orientation := OrientationVertical
          , classes ["scene"]
          ]
    [ -- Child (#expand .== True .+ #fill .== True .+ #padding .== 0) $
      node Label [#label := sceneName]
    , -- Child defaultBoxChildProps $
      container ScrolledWindow [] [renderSequence (applyFocus topSequence focus)]
    ]
