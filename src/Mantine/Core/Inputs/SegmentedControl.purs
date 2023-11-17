module Mantine.Core.Inputs.SegmentedControl
  ( segmentedControl
  , SegmentedControlProps
  , SegmentedControlItem
  , SegmentedControlOrientation(..)
  ) where

import Mantine.Core.Prelude

segmentedControl :: (SegmentedControlProps -> SegmentedControlProps) -> JSX
segmentedControl = mkTrivialComponent segmentedControlComponent

foreign import segmentedControlComponent :: ReactComponent SegmentedControlPropsImpl

type SegmentedControlProps =
  MantineComponent
    ( color                    :: Optional MantineColor
    , data                     :: Array SegmentedControlItem
    , disabled                 :: Boolean
    , fullWidth                :: Boolean
    , name                     :: Optional String
    , orientation              :: SegmentedControlOrientation
    , radius                   :: Optional MantineNumberSize
    , readOnly                 :: Boolean
    , size                     :: Optional MantineSize
    , transitionDuration       :: Milliseconds
    , transitionTimingFunction :: Optional MantineTransitionTimingFunction
    | Controlled String
    )

type SegmentedControlItem =
  { value    :: String
  , label    :: JSX
  , disabled :: Optional Boolean
  }

data SegmentedControlOrientation
  = SegmentedControlOrientationHorizontal
  | SegmentedControlOrientationVertical

instance DefaultValue SegmentedControlOrientation where
  defaultValue = SegmentedControlOrientationHorizontal

type SegmentedControlOrientationImpl = String

instance ToFFI SegmentedControlOrientation SegmentedControlOrientationImpl where
  toNative = case _ of
    SegmentedControlOrientationHorizontal -> "horizontal"
    SegmentedControlOrientationVertical   -> "vertical"

type SegmentedControlPropsImpl =
  MantineComponentImpl
    ( color                    :: OptionalImpl MantineColorImpl
    , data                     :: Array SegmentedControlItemImpl
    , disabled                 :: Boolean
    , fullWidth                :: Boolean
    , name                     :: OptionalImpl String
    , orientation              :: SegmentedControlOrientationImpl
    , radius                   :: OptionalImpl MantineNumberSizeImpl
    , readOnly                 :: Boolean
    , size                     :: OptionalImpl MantineSizeImpl
    , transitionDuration       :: MillisecondsImpl
    , transitionTimingFunction :: OptionalImpl MantineTransitionTimingFunctionImpl
    | ControlledImpl String
    )

type SegmentedControlItemImpl =
  { value    :: String
  , label    :: JSX
  , disabled :: OptionalImpl Boolean
  }
