module Mantine.Core.Miscellaneous.ScrollArea
  ( scrollArea
  , scrollArea_
  , scrollAreaAutosize
  , scrollAreaAutosize_
  , Props_ScrollArea
  , Props_ScrollAreaImpl
  , OffsetScrollbars(..)
  , OffsetScrollbarsImpl
  , ScrollPosition
  , ScrollbarType(..)
  , ScrollbarTypeImpl
  ) where

import Mantine.Core.Prelude
import Web.HTML.HTMLDivElement (HTMLDivElement)

scrollArea
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_ScrollArea
  => Union attrsImpl attrsImpl_ Props_ScrollAreaImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
scrollArea = element (unsafeCoerce scrollAreaComponent) <<< toNative

scrollArea_ :: Array JSX -> JSX
scrollArea_ children = scrollArea { children }

foreign import scrollAreaComponent :: ReactComponent (Record Props_ScrollAreaImpl)

scrollAreaAutosize
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_ScrollArea
  => Union attrsImpl attrsImpl_ Props_ScrollAreaImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
scrollAreaAutosize = element (unsafeCoerce scrollAreaAutosizeComponent) <<< toNative

scrollAreaAutosize_ :: Array JSX -> JSX
scrollAreaAutosize_ children = scrollArea { children }

foreign import scrollAreaAutosizeComponent :: ReactComponent (Record Props_ScrollAreaImpl)

-- Not supported properties
--   { viewportProps :: React.ComponentPropsWithoutRef<"div">
--   }

type Props_ScrollArea =
  Props_Common
    ( children               :: Array JSX
    , offsetScrollbars       :: OffsetScrollbars
    , onScrollPositionChange :: ValueHandler ScrollPosition
    , scrollHideDelay        :: Milliseconds
    , scrollbarSize          :: Pixels
    , type                   :: ScrollbarType
    , viewportRef            :: (Ref HTMLDivElement)
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

type Props_ScrollAreaImpl =
  Props_CommonImpl
    ( children               :: Array JSX
    , offsetScrollbars       :: OffsetScrollbarsImpl
    , onScrollPositionChange :: ValueHandlerImpl ScrollPosition
    , scrollHideDelay        :: MillisecondsImpl
    , scrollbarSize          :: PixelsImpl
    , type                   :: ScrollbarTypeImpl
    , viewportRef            :: (Ref HTMLDivElement)
    )
