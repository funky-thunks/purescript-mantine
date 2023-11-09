module Mantine.Core.Inputs.SegmentedControl
  ( segmentedControl
  , SegmentedControlProps
  , SegmentedControlItem
  , SegmentedControlOrientation(..)
  ) where

import Mantine.Core.Prelude

segmentedControl :: (SegmentedControlProps -> SegmentedControlProps) -> JSX
segmentedControl = mkComponent segmentedControlComponent segmentedControlToImpl defaultMantineComponent_

foreign import segmentedControlComponent :: ReactComponent SegmentedControlPropsImpl

type SegmentedControlProps =
  MantineComponent
    ( color                    :: Maybe MantineColor
    , disabled                 :: Boolean
    , fullWidth                :: Boolean
    , items                    :: Array SegmentedControlItem
    , name                     :: Maybe String
    , orientation              :: SegmentedControlOrientation
    , radius                   :: Maybe MantineNumberSize
    , readOnly                 :: Boolean
    , size                     :: Maybe MantineSize
    , transitionDuration       :: Milliseconds
    , transitionTimingFunction :: Maybe MantineTransitionTimingFunction
    | Controlled String
    )

type SegmentedControlItem =
  { value    :: String
  , label    :: JSX
  , disabled :: Maybe Boolean
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
    ( color                    :: Nullable MantineColorImpl
    , data                     :: Array SegmentedControlItemImpl
    , disabled                 :: Boolean
    , fullWidth                :: Boolean
    , name                     :: Nullable String
    , orientation              :: SegmentedControlOrientationImpl
    , radius                   :: Nullable MantineNumberSizeImpl
    , readOnly                 :: Boolean
    , size                     :: Nullable MantineSizeImpl
    , transitionDuration       :: MillisecondsImpl
    , transitionTimingFunction :: Nullable MantineTransitionTimingFunctionImpl
    | ControlledImpl String
    )

type SegmentedControlItemImpl =
  { value    :: String
  , label    :: JSX
  , disabled :: Nullable Boolean
  }

segmentedControlToImpl :: SegmentedControlProps -> SegmentedControlPropsImpl
segmentedControlToImpl = toNative <<< rename (Proxy :: Proxy "items") (Proxy :: Proxy "data")
