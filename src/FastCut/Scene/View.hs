{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module FastCut.Scene.View (renderScene) where

import           Data.Function ((&))
import           Data.GI.Base.CallStack (HasCallStack)
import qualified Data.HashSet     as HashSet
import           Data.Row.Records hiding (Label, map, focus)
import           Data.Text        (Text)
import           Data.Time.Clock  (NominalDiffTime)

import           FastCut.Focus
import           FastCut.FUI
import           FastCut.Scene hiding (update)
import           FastCut.Sequence

sizeFromDuration :: (RealFrac d) => d -> Size
sizeFromDuration duration =
     #width  .== (fromIntegral (ceiling duration :: Int) * 50)
  .+ #height .== (-1)

focusedClass :: Focused -> Text
focusedClass = \case
  Focused             -> "focused"
  TransitivelyFocused -> "transitively-focused"
  Blurred             -> "blurred"

renderClip' :: Focused -> ClipMetadata -> Object
renderClip' focused metadata =
  box (   #classes .== classes ["clip", focusedClass focused]
       .+ #orientation .== Horizontal
       .+ #size    .== Just (sizeFromDuration (duration metadata))
      )
  [ Child defaultBoxChildProps (label $ #text .== Just (clipName metadata) .+ #classes .== mempty)
  ]

renderGap :: Focused -> NominalDiffTime -> Object
renderGap focused duration =
  box
  (    #classes .== classes ["gap", focusedClass focused]
    .+ #orientation .== Horizontal
    .+ #size    .== Just (sizeFromDuration duration)
  )
  [Child defaultBoxChildProps (label defaultLabelProps)]

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
      box
        (defaultBoxProps
          & update #classes (classes ["sequence", focusedClass focused]))
        (map (Child defaultBoxChildProps . renderSequence) sub)
    Composition focused videoClips audioClips ->
      box
      (defaultBoxProps
          & update #orientation Vertical
          & update #classes (classes ["composition", focusedClass focused]))
        [ Child defaultBoxChildProps $
          box
            (update #classes (classes ["video", focusedClass focused]) defaultBoxProps)
            (map (Child defaultBoxChildProps . renderClip) videoClips)
        , Child defaultBoxChildProps $
          box
            (update #classes (classes ["audio", focusedClass focused]) defaultBoxProps)
            (map (Child defaultBoxChildProps . renderClip) audioClips)
        ]

renderScene :: HasCallStack => Scene -> Object
renderScene Scene {..} =
  box
    sceneBoxProps
    [ Child
        (#expand .== True .+ #fill .== True .+ #padding .== 0)
        (label (#text .== Just sceneName .+ #classes .== mempty))
    , Child
        defaultBoxChildProps
        (scrollArea (renderSequence (applyFocus topSequence focus)))
    ]
  where
    sceneBoxProps :: Rec BoxProps
    sceneBoxProps =
      #orientation .== Vertical .+
      #classes .== classes ["scene"] .+
      #size .== Nothing
