module Mantine.Core.Inputs.Slider
  ( slider
  , SliderProps
  , SliderCommonProps
  , SliderMark

  , rangeSlider
  , RangeSliderProps
  , SliderRange(..)

  , LabelFormatter(..)
  , ScaleFunction(..)
  ) where

import Mantine.Core.Prelude

slider :: (SliderProps -> SliderProps) -> JSX
slider = mkTrivialComponent sliderComponent

foreign import sliderComponent :: ReactComponent SliderPropsImpl

type SliderProps =
  MantineComponent
    ( thumbLabel :: Optional String
    | SliderCommonProps Number
    )

type SliderPropsImpl =
  MantineComponentImpl
    ( thumbLabel :: OptionalImpl String
    | SliderCommonPropsImpl Number
    )

rangeSlider :: (RangeSliderProps -> RangeSliderProps) -> JSX
rangeSlider = mkTrivialComponent rangeSliderComponent

foreign import rangeSliderComponent :: ReactComponent RangeSliderPropsImpl

newtype SliderRange =
  SliderRange
    { from :: Number
    , to   :: Number
    }

instance ToFFI SliderRange (Array Number) where
  toNative (SliderRange { from, to }) = [from, to]

instance FromFFI (Array Number) SliderRange where
  fromNative = case _ of
    [from, to] -> SliderRange { from, to }
    _          -> SliderRange { from: 0.0, to: 0.0 }

type RangeSliderProps =
  MantineComponent
    ( maxRange       :: Optional Number
    , minRange       :: Optional Number
    , thumbFromLabel :: Optional String
    , thumbToLabel   :: Optional String
    | SliderCommonProps SliderRange
    )

type RangeSliderPropsImpl =
  MantineComponentImpl
    ( maxRange       :: OptionalImpl Number
    , minRange       :: OptionalImpl Number
    , thumbFromLabel :: OptionalImpl String
    , thumbToLabel   :: OptionalImpl String
    | SliderCommonPropsImpl (Array Number)
    )

-- Not supported properties
--   { hiddenInputProps :: Omit<DetailedHTMLProps<InputHTMLAttributes<HTMLInputElement>, HTMLInputElement>, "ref">
--   }

type SliderCommonProps value =
  ( color                :: Optional MantineColor
  , disabled             :: Boolean
  , inverted             :: Boolean
  , label                :: Optional LabelFormatter
  , labelAlwaysOn        :: Boolean
  , labelTransitionProps :: MantineTransitionProps
  , marks                :: Array SliderMark
  , max                  :: Optional Number
  , min                  :: Optional Number
  , name                 :: Optional String
  , onChangeEnd          :: ValueHandler value
  , precision            :: Optional Number
  , radius               :: Optional MantineNumberSize
  , scale                :: ScaleFunction
  , showLabelOnHover     :: Boolean
  , size                 :: Optional MantineNumberSize
  , step                 :: Optional Number
  , thumbChildren        :: Optional JSX
  , thumbSize            :: Optional Pixels
  | Controlled value
  )

type SliderMark =
  { value :: Number
  , label :: Optional JSX
  }

newtype LabelFormatter = LabelFormatter (Number -> JSX)

type LabelFormatterImpl = Number -> JSX

instance ToFFI LabelFormatter LabelFormatterImpl where
  toNative (LabelFormatter lf) = lf

newtype ScaleFunction = ScaleFunction (Number -> Number)

instance DefaultValue ScaleFunction where
  defaultValue = ScaleFunction identity

type ScaleFunctionImpl = Number -> Number

instance ToFFI ScaleFunction ScaleFunctionImpl where
  toNative (ScaleFunction sf) = sf

type SliderCommonPropsImpl value =
  ( color                :: OptionalImpl MantineColorImpl
  , disabled             :: Boolean
  , inverted             :: Boolean
  , label                :: OptionalImpl LabelFormatterImpl
  , labelAlwaysOn        :: Boolean
  , labelTransitionProps :: MantineTransitionPropsImpl
  , marks                :: Array SliderMarkImpl
  , max                  :: OptionalImpl Number
  , min                  :: OptionalImpl Number
  , name                 :: OptionalImpl String
  , onChangeEnd          :: ValueHandlerImpl value
  , precision            :: OptionalImpl Number
  , radius               :: OptionalImpl MantineNumberSizeImpl
  , scale                :: ScaleFunctionImpl
  , showLabelOnHover     :: Boolean
  , size                 :: OptionalImpl MantineNumberSizeImpl
  , step                 :: OptionalImpl Number
  , thumbChildren        :: OptionalImpl JSX
  , thumbSize            :: OptionalImpl PixelsImpl
  | ControlledImpl value
  )

type SliderMarkImpl =
  { value :: Number
  , label :: OptionalImpl JSX
  }
