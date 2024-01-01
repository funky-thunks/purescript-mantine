module Mantine.Core.Inputs.SegmentedControl
  ( segmentedControl
  , Props_SegmentedControl
  , Props_SegmentedControlImpl
  , SegmentedControlItem
  , SegmentedControlItemImpl
  , SegmentedControlOrientation(..)
  , SegmentedControlOrientationImpl
  ) where

import Mantine.Core.Prelude

segmentedControl
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_SegmentedControl
  => Union attrsImpl attrsImpl_ Props_SegmentedControlImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
segmentedControl = element (unsafeCoerce segmentedControlComponent) <<< toNative

foreign import segmentedControlComponent :: ReactComponent (Record Props_SegmentedControlImpl)

type Props_SegmentedControl =
  Props_Common
    ( color                    :: MantineColor
    , data                     :: Array SegmentedControlItem
    , disabled                 :: Boolean
    , fullWidth                :: Boolean
    , name                     :: String
    , orientation              :: SegmentedControlOrientation
    , radius                   :: MantineNumberSize
    , readOnly                 :: Boolean
    , size                     :: MantineSize
    , transitionDuration       :: Milliseconds
    , transitionTimingFunction :: MantineTransitionTimingFunction
    | Controlled ValueHandler String
    )

type SegmentedControlItem =
  { value    :: String
  , label    :: JSX
  , disabled :: Optional Boolean
  }

data SegmentedControlOrientation
  = SegmentedControlOrientationHorizontal
  | SegmentedControlOrientationVertical

type SegmentedControlOrientationImpl = String

instance ToFFI SegmentedControlOrientation SegmentedControlOrientationImpl where
  toNative = case _ of
    SegmentedControlOrientationHorizontal -> "horizontal"
    SegmentedControlOrientationVertical   -> "vertical"

type Props_SegmentedControlImpl =
  Props_CommonImpl
    ( color                    :: MantineColorImpl
    , data                     :: Array SegmentedControlItemImpl
    , disabled                 :: Boolean
    , fullWidth                :: Boolean
    , name                     :: String
    , orientation              :: SegmentedControlOrientationImpl
    , radius                   :: MantineNumberSizeImpl
    , readOnly                 :: Boolean
    , size                     :: MantineSizeImpl
    , transitionDuration       :: MillisecondsImpl
    , transitionTimingFunction :: MantineTransitionTimingFunctionImpl
    | ControlledImpl ValueHandlerImpl String
    )

type SegmentedControlItemImpl =
  { value    :: String
  , label    :: JSX
  , disabled :: OptionalImpl Boolean
  }
