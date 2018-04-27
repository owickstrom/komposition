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
import           Data.Row.Records hiding (Label, focus, map)
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
  gtk Box [classes ["clip", focusedClass focused]
          , #orientation := OrientationHorizontal
          , #widthRequest := widthFromDuration (duration metadata)
          ]
  -- [ Child defaultBoxChildProps $
  --   gtk Label [#label := (clipName metadata), cssClass "clip"]
  -- ]

renderGap :: Focused -> NominalDiffTime -> Object
renderGap focused duration =
  gtk Box
  [    classes ["gap", focusedClass focused]
  , #orientation := OrientationHorizontal
  , #widthRequest := widthFromDuration duration
  ]
  -- [Child defaultBoxChildProps (gtk Label [])]

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
      gtk Box
        [ classes ["sequence", focusedClass focused]
        ]
        -- (map (Child defaultBoxChildProps . renderSequence) sub)
    Composition focused videoClips audioClips ->
      gtk Box
          [#orientation := OrientationVertical
          , classes ["composition", focusedClass focused]
          ]
        {-
        [ Child defaultBoxChildProps $
          box
            (update #classes (classes ["video", focusedClass focused]) defaultBoxProps)
            (map (Child defaultBoxChildProps . renderClip) videoClips)
        , Child defaultBoxChildProps $
          box
            (update #classes (classes ["audio", focusedClass focused]) defaultBoxProps)
            (map (Child defaultBoxChildProps . renderClip) audioClips)
        ]
        -}

renderScene :: Scene -> Object
renderScene Scene {..} =
  gtk Button [ #label := "YES!"]
  --gtk Box [ #orientation := OrientationVertical
  --        , classes ["scene"]
  --        ]
  {-
    [ Child
        (#expand .== True .+ #fill .== True .+ #padding .== 0)
        (gtk Label [#label := sceneName])
    , Child
        defaultBoxChildProps
        (scrollArea (renderSequence (applyFocus topSequence focus)))
    ]
-}
