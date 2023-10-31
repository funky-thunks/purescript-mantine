module Mantine.Core.Miscellaneous.ScrollArea
  ( scrollArea
  , scrollArea_
  , scrollAreaAutosize
  , scrollAreaAutosize_
  , ScrollAreaProps
  , OffsetScrollbars(..)
  , ScrollPosition
  , ScrollbarType(..)
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

-- Not supported properties
--   { viewportProps :: React.ComponentPropsWithoutRef<"div">
--   }

type ScrollAreaProps =
  MantineComponent
    ( children               :: Array JSX
    , offsetScrollbars       :: Maybe OffsetScrollbars
    , onScrollPositionChange :: ValueHandler ScrollPosition
    , scrollHideDelay        :: Maybe Milliseconds
    , scrollbarSize          :: Maybe Pixels
    , type                   :: Maybe ScrollbarType
    , viewportRef            :: Maybe (Ref HTMLDivElement)
    )

data OffsetScrollbars
  = OffsetScrollbarsAllDirection
  | OffsetScrollbarsXAxis
  | OffsetScrollbarsYAxis

type OffsetScrollbarsImpl = Boolean |+| String

instance ToFFI OffsetScrollbars OffsetScrollbarsImpl where
  toNative = case _ of
    OffsetScrollbarsAllDirection -> asOneOf true
    OffsetScrollbarsXAxis        -> asOneOf "x"
    OffsetScrollbarsYAxis        -> asOneOf "y"

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

type ScrollbarTypeImpl = String

instance ToFFI ScrollbarType ScrollbarTypeImpl where
  toNative = case _ of
    Auto   -> "auto"
    Scroll -> "scroll"
    Always -> "always"
    Never  -> "never"
    Hover  -> "hover"

type ScrollAreaPropsImpl =
  MantineComponentImpl
    ( children               :: Array JSX
    , offsetScrollbars       :: Nullable OffsetScrollbarsImpl
    , onScrollPositionChange :: ValueHandlerImpl ScrollPosition
    , scrollHideDelay        :: Nullable MillisecondsImpl
    , scrollbarSize          :: Nullable PixelsImpl
    , type                   :: Nullable ScrollbarTypeImpl
    , viewportRef            :: Nullable (Ref HTMLDivElement)
    )
