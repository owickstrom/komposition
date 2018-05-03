{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module FastCut.Project.Controller
  ( Controller
  , makeController
  , Event(..)
  , update
  , render
  ) where

import           Control.Lens
import           Data.Int
import           Data.Text          (Text)
import           Data.Time.Clock    (NominalDiffTime)
import           GI.Gtk             hiding ((:=))

import           FastCut.Focus
import           FastCut.Project
import           FastCut.Sequence
import           GI.Gtk.Declarative

data State
  = InSequence
  | SelectingLibraryClip ClipType Int
  deriving (Eq, Show)

data Controller = Controller
  { _project :: Project
  , _state   :: State
  , _focus   :: Focus
  } deriving (Eq, Show)

makeLenses ''Controller

makeController :: Project -> Controller
makeController p =
  Controller p InSequence (InSequenceFocus 0 Nothing)

data Event
  = FocusEvent FocusEvent
  | OpenLibrary
  | Append
  | Cancel
  deriving (Eq, Show)

update :: Controller -> Event -> Controller
update controller event =
  case (controller ^. state, event) of
    (InSequence, FocusEvent focusEvent) ->
      -- TODO: Fix this ugly mess. How to transform the focus and get
      -- the Either on "the outside"?
      case modifyFocus
             (controller ^. project.topSequence)
             focusEvent
             (controller ^. focus) of
        Left _       -> controller
        Right focus' -> controller & focus .~ focus'
    -- TODO: Figure out what kind of clips to show.
    (InSequence, OpenLibrary) -> controller & state .~ SelectingLibraryClip Video 0
    (SelectingLibraryClip{}, Append) -> controller & project.topSequence %~ appendAt (controller ^. focus)
    (SelectingLibraryClip{}, Cancel) -> controller & state .~ InSequence
    _ -> controller

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
    Composition focused vs as ->
      container
        Box
        [ #orientation := OrientationVertical
        , classes ["composition", focusedClass focused]
        ]
        [ BoxChild False False 0 $
          container
            Box
            [classes ["video", focusedClass focused]]
            (map renderClip vs)
        , BoxChild False False 0 $
          container
            Box
            [classes ["audio", focusedClass focused]]
            (map renderClip as)
        ]

renderLibrary :: Library -> ClipType -> Int -> Object
renderLibrary library clipType idx =
  case clipType of
    Video -> renderClips (library ^. videoClips) idx
    Audio -> renderClips (library ^. audioClips) idx
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
        VideoGap _ d -> node Label [#label := "GAP?"]
        AudioGap _ d -> node Label [#label := "GAP?"]

render :: Controller -> Object
render controller =
  case controller ^. state of
    InSequence ->
      container
        Box
        [#orientation := OrientationVertical, classes ["scene"]]
        [ BoxChild True True 0 $
          node Label [#label := (controller ^. project . projectName)]
        , BoxChild False False 0 $
          container
            ScrolledWindow
            [ #hscrollbarPolicy := PolicyTypeAutomatic
            , #vscrollbarPolicy := PolicyTypeNever
            ]
            (renderSequence
               (applyFocus
                  (controller ^. project . topSequence)
                  (controller ^. focus)))
        ]
    SelectingLibraryClip clipType idx ->
      renderLibrary (controller ^. project.library) clipType idx
