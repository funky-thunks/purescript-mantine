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
  ThemingProps
    ( SliderCommonProps Number +
        ( thumbLabel :: Maybe String
        )
    )

type SliderPropsImpl =
  ThemingPropsImpl
    ( SliderCommonPropsImpl Number +
        ( thumbLabel :: Nullable String
        )
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
  ThemingProps
    ( SliderCommonProps SliderRange +
        ( maxRange       :: Maybe Number
        , minRange       :: Maybe Number
        , thumbFromLabel :: Maybe String
        , thumbToLabel   :: Maybe String
        )
    )

type RangeSliderPropsImpl =
  ThemingPropsImpl
    ( SliderCommonPropsImpl (Array Number) +
        ( maxRange       :: Nullable Number
        , minRange       :: Nullable Number
        , thumbFromLabel :: Nullable String
        , thumbToLabel   :: Nullable String
        )
    )

type SliderCommonProps value r =
  ( color                         :: Maybe MantineColor
  , defaultValue                  :: Maybe value
  , disabled                      :: Boolean
  , inverted                      :: Boolean
  , label                         :: Maybe LabelFormatter
  , labelAlwaysOn                 :: Boolean
  , labelTransition               :: Maybe MantineTransition
  , labelTransitionDuration       :: Maybe Milliseconds
  , labelTransitionTimingFunction :: Maybe MantineTransitionTimingFunction
  , marks                         :: Array SliderMark
  , max                           :: Maybe Number
  , min                           :: Maybe Number
  , name                          :: Maybe String
  , onChange                      :: ValueHandler value
  , onChangeEnd                   :: ValueHandler value
  , precision                     :: Maybe Number
  , radius                        :: Maybe MantineNumberSize
  , scale                         :: ScaleFunction
  , showLabelOnHover              :: Boolean
  , size                          :: Maybe MantineNumberSize
  , step                          :: Maybe Number
  , thumbChildren                 :: Maybe JSX
  , thumbSize                     :: Maybe Pixels
  , value                         :: Maybe value
  | r
  )

newtype LabelFormatter = LabelFormatter (Number -> JSX)

instance ToFFI LabelFormatter (Number -> JSX) where
  toNative (LabelFormatter lf) = lf

newtype ScaleFunction = ScaleFunction (Number -> Number)

instance DefaultValue ScaleFunction where
  defaultValue = ScaleFunction identity

instance ToFFI ScaleFunction (Number -> Number) where
  toNative (ScaleFunction sf) = sf

type SliderCommonPropsImpl value r =
  ( color                         :: Nullable String
  , defaultValue                  :: Nullable value
  , disabled                      :: Boolean
  , inverted                      :: Boolean
  , label                         :: Nullable (Number -> JSX)
  , labelAlwaysOn                 :: Boolean
  , labelTransition               :: Nullable String
  , labelTransitionDuration       :: Nullable Number
  , labelTransitionTimingFunction :: Nullable String
  , marks                         :: Array SliderMarkImpl
  , max                           :: Nullable Number
  , min                           :: Nullable Number
  , name                          :: Nullable String
  , onChange                      :: EffectFn1 value Unit
  , onChangeEnd                   :: EffectFn1 value Unit
  , precision                     :: Nullable Number
  , radius                        :: Nullable MantineNumberSizeImpl
  , scale                         :: Number -> Number
  , showLabelOnHover              :: Boolean
  , size                          :: Nullable MantineNumberSizeImpl
  , step                          :: Nullable Number
  , thumbChildren                 :: Nullable JSX
  , thumbSize                     :: Nullable Number
  , value                         :: Nullable value
  | r
  )

type SliderMark =
  { value :: Number
  , label :: Maybe JSX
  }

type SliderMarkImpl =
  { value :: Number
  , label :: Nullable JSX
  }
