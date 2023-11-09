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
    ( active        :: Maybe Int
    , align         :: TimelineAlign
    , bulletSize    :: Maybe Pixels
    , children      :: Array JSX
    , color         :: Maybe MantineColor
    , lineWidth     :: Maybe Pixels
    , radius        :: Maybe MantineNumberSize
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
    ( active        :: Nullable Number
    , align         :: TimelineAlignImpl
    , bulletSize    :: Nullable PixelsImpl
    , children      :: Array JSX
    , color         :: Nullable MantineColorImpl
    , lineWidth     :: Nullable PixelsImpl
    , radius        :: Nullable MantineNumberSizeImpl
    , reverseActive :: Boolean
    )

timelineItem :: (TimelineItemProps -> TimelineItemProps) -> JSX
timelineItem = mkTrivialComponent timelineItemComponent

foreign import timelineItemComponent :: ReactComponent TimelineItemPropsImpl

type TimelineItemProps =
  MantineComponent
    ( bullet      :: Maybe JSX
    , children    :: Array JSX
    , color       :: Maybe MantineColor
    , lineVariant :: TimelineLineVariant
    , radius      :: Maybe MantineNumberSize
    , title       :: Maybe JSX
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
    ( bullet      :: Nullable JSX
    , children    :: Array JSX
    , color       :: Nullable MantineColorImpl
    , lineVariant :: TimelineLineVariantImpl
    , radius      :: Nullable MantineNumberSizeImpl
    , title       :: Nullable JSX
    )
