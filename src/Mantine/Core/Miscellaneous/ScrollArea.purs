module Mantine.Core.Miscellaneous.ScrollArea
  ( scrollArea
  , scrollArea_
  , ScrollAreaProps
  , ScrollPosition
  , ScrollbarType(..)
  ) where

import Prelude (Unit)
import Mantine.Core.Prelude
import Web.HTML.HTMLDivElement (HTMLDivElement)

scrollArea :: (ScrollAreaProps -> ScrollAreaProps) -> JSX
scrollArea = mkTrivialComponent scrollAreaComponent

scrollArea_ :: Array JSX -> JSX
scrollArea_ children = scrollArea _ { children = children }

foreign import scrollAreaComponent :: ReactComponent ScrollAreaPropsImpl

type ScrollAreaProps =
  ThemingProps
    ( children               :: Array JSX
    , offsetScrollbars       :: Maybe Boolean
    , onScrollPositionChange :: Maybe (ScrollPosition -> Effect Unit)
    , scrollHideDelay        :: Maybe Milliseconds
    , scrollbarSize          :: Maybe Pixels
    , type                   :: Maybe ScrollbarType
    , viewportRef            :: Maybe (Ref HTMLDivElement)
    )

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
    , offsetScrollbars       :: Nullable Boolean
    , onScrollPositionChange :: Nullable (EffectFn1 ScrollPosition Unit)
    , scrollHideDelay        :: Nullable Milliseconds
    , scrollbarSize          :: Nullable Pixels
    , type                   :: Nullable String
    , viewportRef            :: Nullable (Ref HTMLDivElement)
    )
