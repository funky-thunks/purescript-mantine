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
    , defaultValue             :: Maybe String
    , disabled                 :: Boolean
    , fullWidth                :: Boolean
    , items                    :: Array SegmentedControlItem
    , name                     :: Maybe String
    , onChange                 :: ValueHandler String
    , orientation              :: SegmentedControlOrientation
    , radius                   :: Maybe MantineNumberSize
    , readOnly                 :: Boolean
    , size                     :: Maybe MantineSize
    , transitionDuration       :: Milliseconds
    , transitionTimingFunction :: Maybe MantineTransitionTimingFunction
    , value                    :: Maybe String
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
    , defaultValue             :: Nullable String
    , disabled                 :: Boolean
    , fullWidth                :: Boolean
    , name                     :: Nullable String
    , onChange                 :: ValueHandlerImpl String
    , orientation              :: SegmentedControlOrientationImpl
    , radius                   :: Nullable MantineNumberSizeImpl
    , readOnly                 :: Boolean
    , size                     :: Nullable MantineSizeImpl
    , transitionDuration       :: MillisecondsImpl
    , transitionTimingFunction :: Nullable MantineTransitionTimingFunctionImpl
    , value                    :: Nullable String
    )

type SegmentedControlItemImpl =
  { value    :: String
  , label    :: JSX
  , disabled :: Nullable Boolean
  }

segmentedControlToImpl :: SegmentedControlProps -> SegmentedControlPropsImpl
segmentedControlToImpl = toNative <<< rename (Proxy :: Proxy "items") (Proxy :: Proxy "data")
