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
    ( thumbLabel :: Maybe String
    | SliderCommonProps Number
    )

type SliderPropsImpl =
  MantineComponentImpl
    ( thumbLabel :: Nullable String
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
    ( maxRange       :: Maybe Number
    , minRange       :: Maybe Number
    , thumbFromLabel :: Maybe String
    , thumbToLabel   :: Maybe String
    | SliderCommonProps SliderRange
    )

type RangeSliderPropsImpl =
  MantineComponentImpl
    ( maxRange       :: Nullable Number
    , minRange       :: Nullable Number
    , thumbFromLabel :: Nullable String
    , thumbToLabel   :: Nullable String
    | SliderCommonPropsImpl (Array Number)
    )

-- Not supported properties
--   { hiddenInputProps :: Omit<DetailedHTMLProps<InputHTMLAttributes<HTMLInputElement>, HTMLInputElement>, "ref">
--   }

type SliderCommonProps value =
  ( color                :: Maybe MantineColor
  , disabled             :: Boolean
  , inverted             :: Boolean
  , label                :: Maybe LabelFormatter
  , labelAlwaysOn        :: Boolean
  , labelTransitionProps :: MantineTransitionProps
  , marks                :: Array SliderMark
  , max                  :: Maybe Number
  , min                  :: Maybe Number
  , name                 :: Maybe String
  , onChangeEnd          :: ValueHandler value
  , precision            :: Maybe Number
  , radius               :: Maybe MantineNumberSize
  , scale                :: ScaleFunction
  , showLabelOnHover     :: Boolean
  , size                 :: Maybe MantineNumberSize
  , step                 :: Maybe Number
  , thumbChildren        :: Maybe JSX
  , thumbSize            :: Maybe Pixels
  | Controlled value
  )

type SliderMark =
  { value :: Number
  , label :: Maybe JSX
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
  ( color                :: Nullable MantineColorImpl
  , disabled             :: Boolean
  , inverted             :: Boolean
  , label                :: Nullable LabelFormatterImpl
  , labelAlwaysOn        :: Boolean
  , labelTransitionProps :: MantineTransitionPropsImpl
  , marks                :: Array SliderMarkImpl
  , max                  :: Nullable Number
  , min                  :: Nullable Number
  , name                 :: Nullable String
  , onChangeEnd          :: ValueHandlerImpl value
  , precision            :: Nullable Number
  , radius               :: Nullable MantineNumberSizeImpl
  , scale                :: ScaleFunctionImpl
  , showLabelOnHover     :: Boolean
  , size                 :: Nullable MantineNumberSizeImpl
  , step                 :: Nullable Number
  , thumbChildren        :: Nullable JSX
  , thumbSize            :: Nullable PixelsImpl
  | ControlledImpl value
  )

type SliderMarkImpl =
  { value :: Number
  , label :: Nullable JSX
  }
