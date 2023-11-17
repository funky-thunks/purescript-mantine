module Mantine.Core.DataDisplay.Timeline
  ( timeline
  , Props_Timeline
  , Props_TimelineImpl
  , TimelineAlign(..)
  , TimelineAlignImpl

  , timelineItem
  , Props_TimelineItem
  , Props_TimelineItemImpl
  , TimelineLineVariant(..)
  , TimelineLineVariantImpl
  ) where

import Mantine.Core.Prelude

timeline
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Timeline
  => Union attrsImpl attrsImpl_ Props_TimelineImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
timeline = element (unsafeCoerce timelineComponent) <<< toNative

foreign import timelineComponent :: ReactComponent (Record Props_TimelineImpl)

type Props_Timeline =
  Props_Common
    ( active        :: Int
    , align         :: TimelineAlign
    , bulletSize    :: Pixels
    , children      :: Array JSX
    , color         :: MantineColor
    , lineWidth     :: Pixels
    , radius        :: MantineNumberSize
    , reverseActive :: Boolean
    )

data TimelineAlign
  = TimelineAlignLeft
  | TimelineAlignRight

type TimelineAlignImpl = String

instance ToFFI TimelineAlign TimelineAlignImpl where
  toNative = case _ of
    TimelineAlignLeft  -> "left"
    TimelineAlignRight -> "right"

type Props_TimelineImpl =
  Props_CommonImpl
    ( active        :: Number
    , align         :: TimelineAlignImpl
    , bulletSize    :: PixelsImpl
    , children      :: Array JSX
    , color         :: MantineColorImpl
    , lineWidth     :: PixelsImpl
    , radius        :: MantineNumberSizeImpl
    , reverseActive :: Boolean
    )

timelineItem
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_TimelineItem
  => Union attrsImpl attrsImpl_ Props_TimelineItemImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
timelineItem = element (unsafeCoerce timelineItemComponent) <<< toNative

foreign import timelineItemComponent :: ReactComponent (Record Props_TimelineItemImpl)

type Props_TimelineItem =
  Props_Common
    ( bullet      :: JSX
    , children    :: Array JSX
    , color       :: MantineColor
    , lineVariant :: TimelineLineVariant
    , radius      :: MantineNumberSize
    , title       :: JSX
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

type Props_TimelineItemImpl =
  Props_CommonImpl
    ( bullet      :: JSX
    , children    :: Array JSX
    , color       :: MantineColorImpl
    , lineVariant :: TimelineLineVariantImpl
    , radius      :: MantineNumberSizeImpl
    , title       :: JSX
    )
