module Mantine.Core.DataDisplay.Timeline
  ( timeline
  , TimelineProps
  , TimelineAlign(..)

  , timelineItem
  , TimelineItemProps
  , TimelineLineVariant(..)
  ) where

import Mantine.Core.Prelude

timeline :: (TimelineProps -> TimelineProps) -> JSX
timeline = mkTrivialComponent timelineComponent

foreign import timelineComponent :: ReactComponent TimelinePropsImpl

type TimelineProps =
  MantineComponent
    ( active        :: Optional Int
    , align         :: TimelineAlign
    , bulletSize    :: Optional Pixels
    , children      :: Array JSX
    , color         :: Optional MantineColor
    , lineWidth     :: Optional Pixels
    , radius        :: Optional MantineNumberSize
    , reverseActive :: Boolean
    )

data TimelineAlign
  = TimelineAlignLeft
  | TimelineAlignRight

instance DefaultValue TimelineAlign where defaultValue = TimelineAlignLeft

type TimelineAlignImpl = String

instance ToFFI TimelineAlign TimelineAlignImpl where
  toNative = case _ of
    TimelineAlignLeft  -> "left"
    TimelineAlignRight -> "right"

type TimelinePropsImpl =
  MantineComponentImpl
    ( active        :: OptionalImpl Number
    , align         :: TimelineAlignImpl
    , bulletSize    :: OptionalImpl PixelsImpl
    , children      :: Array JSX
    , color         :: OptionalImpl MantineColorImpl
    , lineWidth     :: OptionalImpl PixelsImpl
    , radius        :: OptionalImpl MantineNumberSizeImpl
    , reverseActive :: Boolean
    )

timelineItem :: (TimelineItemProps -> TimelineItemProps) -> JSX
timelineItem = mkTrivialComponent timelineItemComponent

foreign import timelineItemComponent :: ReactComponent TimelineItemPropsImpl

type TimelineItemProps =
  MantineComponent
    ( bullet      :: Optional JSX
    , children    :: Array JSX
    , color       :: Optional MantineColor
    , lineVariant :: TimelineLineVariant
    , radius      :: Optional MantineNumberSize
    , title       :: Optional JSX
    )

data TimelineLineVariant
  = TimelineLineVariantDashed
  | TimelineLineVariantDotted
  | TimelineLineVariantSolid

type TimelineLineVariantImpl = String

instance ToFFI TimelineLineVariant TimelineLineVariantImpl where
  toNative = case _ of
    TimelineLineVariantDashed -> "dashed"
    TimelineLineVariantDotted -> "dotted"
    TimelineLineVariantSolid  -> "solid"

instance DefaultValue TimelineLineVariant where defaultValue = TimelineLineVariantSolid

type TimelineItemPropsImpl =
  MantineComponentImpl
    ( bullet      :: OptionalImpl JSX
    , children    :: Array JSX
    , color       :: OptionalImpl MantineColorImpl
    , lineVariant :: TimelineLineVariantImpl
    , radius      :: OptionalImpl MantineNumberSizeImpl
    , title       :: OptionalImpl JSX
    )
