module Mantine.Core.Miscellaneous.ScrollArea
  ( scrollArea
  , scrollArea_
  , scrollAreaAutosize
  , scrollAreaAutosize_
  , ScrollAreaProps
  , ScrollPosition
  , ScrollbarType(..)
  , ReadingDirection(..)
  ) where

import Mantine.Core.Prelude
import Web.HTML.HTMLDivElement (HTMLDivElement)

scrollArea :: (ScrollAreaProps -> ScrollAreaProps) -> JSX
scrollArea = mkTrivialComponent scrollAreaComponent

scrollArea_ :: Array JSX -> JSX
scrollArea_ children = scrollArea _ { children = children }

foreign import scrollAreaComponent :: ReactComponent ScrollAreaPropsImpl

scrollAreaAutosize :: (ScrollAreaProps -> ScrollAreaProps) -> JSX
scrollAreaAutosize = mkTrivialComponent scrollAreaAutosizeComponent

scrollAreaAutosize_ :: Array JSX -> JSX
scrollAreaAutosize_ children = scrollArea _ { children = children }

foreign import scrollAreaAutosizeComponent :: ReactComponent ScrollAreaPropsImpl

type ScrollAreaProps =
  ThemingProps
    ( children               :: Array JSX
    , dir                    :: Maybe ReadingDirection
    , offsetScrollbars       :: Maybe Boolean
    , onScrollPositionChange :: Maybe (ScrollPosition -> Effect Unit)
    , scrollHideDelay        :: Maybe Milliseconds
    , scrollbarSize          :: Maybe Pixels
    , type                   :: Maybe ScrollbarType
    , viewportRef            :: Maybe (Ref HTMLDivElement)
    )

data ReadingDirection = LTR | RTL

instance ToFFI ReadingDirection String where
  toNative = case _ of
    LTR -> "ltr"
    RTL -> "rtl"

type ScrollPosition =
  { x :: Number
  , y :: Number
  }

data ScrollbarType
  = Auto
  | Scroll
  | Always
  | Never
  | Hover

instance ToFFI ScrollbarType String where
  toNative = case _ of
    Auto   -> "auto"
    Scroll -> "scroll"
    Always -> "always"
    Never  -> "never"
    Hover  -> "hover"

type ScrollAreaPropsImpl =
  ThemingPropsImpl
    ( children               :: Array JSX
    , dir                    :: Nullable String
    , offsetScrollbars       :: Nullable Boolean
    , onScrollPositionChange :: Nullable (EffectFn1 ScrollPosition Unit)
    , scrollHideDelay        :: Nullable Milliseconds
    , scrollbarSize          :: Nullable Pixels
    , type                   :: Nullable String
    , viewportRef            :: Nullable (Ref HTMLDivElement)
    )
