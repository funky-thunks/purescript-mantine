module Mantine.Core.Inputs.SegmentedControl
  ( segmentedControl
  , SegmentedControlProps
  , SegmentedControlItem
  , SegmentedControlOrientation(..)
  ) where

import Mantine.Core.Prelude

segmentedControl :: (SegmentedControlProps -> SegmentedControlProps) -> JSX
segmentedControl = mkComponent segmentedControlComponent segmentedControlToImpl defaultThemingProps_

foreign import segmentedControlComponent :: ReactComponent SegmentedControlPropsImpl

type SegmentedControlProps =
  ThemingProps
    ( color                    :: Maybe MantineColor
    , items                    :: Array SegmentedControlItem
    , defaultValue             :: Maybe String
    , disabled                 :: Boolean
    , fullWidth                :: Boolean
    , name                     :: Maybe String
    , onChange                 :: ValueHandler String
    , orientation              :: SegmentedControlOrientation
    , radius                   :: Maybe MantineNumberSize
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

instance ToFFI SegmentedControlOrientation String where
  toNative = case _ of
    SegmentedControlOrientationHorizontal -> "horizontal"
    SegmentedControlOrientationVertical   -> "vertical"

type SegmentedControlPropsImpl =
  ThemingPropsImpl
    ( color                    :: Nullable String
    , data                     :: Array SegmentedControlItemImpl
    , defaultValue             :: Nullable String
    , disabled                 :: Boolean
    , fullWidth                :: Boolean
    , name                     :: Nullable String
    , onChange                 :: EffectFn1 String Unit
    , orientation              :: String
    , radius                   :: Nullable MantineNumberSizeImpl
    , size                     :: Nullable String
    , transitionDuration       :: Number
    , transitionTimingFunction :: Nullable String
    , value                    :: Nullable String
    )

type SegmentedControlItemImpl =
  { value    :: String
  , label    :: JSX
  , disabled :: Nullable Boolean
  }

segmentedControlToImpl :: SegmentedControlProps -> SegmentedControlPropsImpl
segmentedControlToImpl = toNative <<< rename (Proxy :: Proxy "items") (Proxy :: Proxy "data")
