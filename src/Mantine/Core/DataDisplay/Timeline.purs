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
  ThemingProps
    ( active        :: Maybe Int
    , align         :: TimelineAlign
    , bulletSize    :: Maybe Pixels
    , children      :: Array JSX
    , color         :: Maybe MantineColor
    , lineWidth     :: Maybe Pixels
    , radius        :: Maybe MantineNumberSize
    , reverseActive :: Boolean
    )

data TimelineAlign = TimelineAlignLeft | TimelineAlignRight

instance DefaultValue TimelineAlign where defaultValue = TimelineAlignLeft

instance ToFFI TimelineAlign String where
  toNative = case _ of
    TimelineAlignLeft  -> "left"
    TimelineAlignRight -> "right"

type TimelinePropsImpl =
  ThemingPropsImpl
    ( active        :: Nullable Number
    , align         :: String
    , bulletSize    :: Nullable Number
    , children      :: Array JSX
    , color         :: Nullable String
    , lineWidth     :: Nullable Number
    , radius        :: Nullable MantineNumberSizeImpl
    , reverseActive :: Boolean
    )

timelineItem :: (TimelineItemProps -> TimelineItemProps) -> JSX
timelineItem = mkTrivialComponent timelineItemComponent

foreign import timelineItemComponent :: ReactComponent TimelineItemPropsImpl

type TimelineItemProps =
  ThemingProps
    ( active      :: Boolean
    , align       :: TimelineAlign
    , bullet      :: Maybe JSX
    , bulletSize  :: Maybe Pixels
    , children    :: Array JSX
    , color       :: Maybe MantineColor
    , lineActive  :: Boolean
    , lineVariant :: TimelineLineVariant
    , lineWidth   :: Maybe Pixels
    , radius      :: Maybe MantineNumberSize
    , title       :: Maybe JSX
    )

data TimelineLineVariant
  = TimelineLineVariantDashed
  | TimelineLineVariantDotted
  | TimelineLineVariantSolid

instance ToFFI TimelineLineVariant String where
  toNative = case _ of
    TimelineLineVariantDashed -> "dashed"
    TimelineLineVariantDotted -> "dotted"
    TimelineLineVariantSolid  -> "solid"

instance DefaultValue TimelineLineVariant where defaultValue = TimelineLineVariantSolid

type TimelineItemPropsImpl =
  ThemingPropsImpl
    ( active      :: Boolean
    , align       :: String
    , bullet      :: Nullable JSX
    , bulletSize  :: Nullable Number
    , children    :: Array JSX
    , color       :: Nullable String
    , lineActive  :: Boolean
    , lineVariant :: String
    , lineWidth   :: Nullable Number
    , radius      :: Nullable MantineNumberSizeImpl
    , title       :: Nullable JSX
    )
