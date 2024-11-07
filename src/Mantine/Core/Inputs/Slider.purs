module Mantine.Core.Inputs.Slider
  ( slider
  , Props_Slider
  , Props_SliderImpl
  , Props_SliderCommon
  , Props_SliderCommonImpl
  , SliderMark
  , SliderMarkImpl

  , rangeSlider
  , Props_RangeSlider
  , Props_RangeSliderImpl
  , SliderRange(..)
  ) where

import Mantine.Core.Prelude

slider
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Slider
  => Union attrsImpl attrsImpl_ Props_SliderImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
slider = element (unsafeCoerce sliderComponent) <<< toNative

foreign import sliderComponent :: ReactComponent (Record Props_SliderImpl)

type Props_Slider =
  Props_Common
    ( thumbLabel :: String
    | Props_SliderCommon Number
    )

type Props_SliderImpl =
  Props_CommonImpl
    ( thumbLabel :: String
    | Props_SliderCommonImpl Number
    )

rangeSlider
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_RangeSlider
  => Union attrsImpl attrsImpl_ Props_RangeSliderImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
rangeSlider = element (unsafeCoerce rangeSliderComponent) <<< toNative

foreign import rangeSliderComponent :: ReactComponent (Record Props_RangeSliderImpl)

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

type Props_RangeSlider =
  Props_Common
    ( maxRange       :: Number
    , minRange       :: Number
    , thumbFromLabel :: String
    , thumbToLabel   :: String
    | Props_SliderCommon SliderRange
    )

type Props_RangeSliderImpl =
  Props_CommonImpl
    ( maxRange       :: Number
    , minRange       :: Number
    , thumbFromLabel :: String
    , thumbToLabel   :: String
    | Props_SliderCommonImpl (Array Number)
    )

-- Not supported properties
--   { hiddenInputProps :: Omit<DetailedHTMLProps<InputHTMLAttributes<HTMLInputElement>, HTMLInputElement>, "ref">
--   }

type Props_SliderCommon value =
  ( color                :: MantineColor
  , disabled             :: Boolean
  , inverted             :: Boolean
  , label                :: Number -> JSX
  , labelAlwaysOn        :: Boolean
  , labelTransitionProps :: MantineTransitionProps
  , marks                :: Array SliderMark
  , max                  :: Number
  , min                  :: Number
  , name                 :: String
  , onChangeEnd          :: ValueHandler value
  , precision            :: Number
  , radius               :: MantineNumberSize
  , scale                :: Number -> Number
  , showLabelOnHover     :: Boolean
  , size                 :: MantineNumberSize
  , step                 :: Number
  , thumbChildren        :: JSX
  , thumbSize            :: Pixels
  | Controlled ValueHandler value
  )

type SliderMark =
  { value :: Number
  , label :: Optional JSX
  }

type Props_SliderCommonImpl value =
  ( color                :: MantineColorImpl
  , disabled             :: Boolean
  , inverted             :: Boolean
  , label                :: Number -> JSX
  , labelAlwaysOn        :: Boolean
  , labelTransitionProps :: MantineTransitionPropsImpl
  , marks                :: Array SliderMarkImpl
  , max                  :: Number
  , min                  :: Number
  , name                 :: String
  , onChangeEnd          :: ValueHandlerImpl value
  , precision            :: Number
  , radius               :: MantineNumberSizeImpl
  , scale                :: Number -> Number
  , showLabelOnHover     :: Boolean
  , size                 :: MantineNumberSizeImpl
  , step                 :: Number
  , thumbChildren        :: JSX
  , thumbSize            :: PixelsImpl
  | ControlledImpl ValueHandlerImpl value
  )

type SliderMarkImpl =
  { value :: Number
  , label :: OptionalImpl JSX
  }
