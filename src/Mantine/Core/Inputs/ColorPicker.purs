module Mantine.Core.Inputs.ColorPicker
  ( colorPicker
  , Props_ColorPicker
  , Props_ColorPickerImpl
  , ColorFormat(..)
  , ColorFormatImpl
  , ColorFormula(..)
  , ColorFormulaImpl

  , alphaSlider
  , Props_AlphaSlider
  , Props_AlphaSliderImpl

  , hueSlider
  , Props_HueSlider
  , Props_HueSliderImpl

  , ColorPicking
  , ColorPickingImpl
  ) where

import Mantine.Core.Prelude

colorPicker
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_ColorPicker
  => Union attrsImpl attrsImpl_ Props_ColorPickerImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
colorPicker = element (unsafeCoerce colorPickerComponent) <<< toNative

foreign import colorPickerComponent :: ReactComponent (Record Props_ColorPickerImpl)

type ColorPicking =
  ( format         :: ColorFormat
  , onChangeEnd    :: ValueHandler ColorFormula
  , swatches       :: Array ColorFormula
  , swatchesPerRow :: Int
  , withPicker     :: Boolean
  | Controlled ColorFormula
  )

type Props_ColorPicker =
  Props_Common
    ( alphaLabel         :: String
    , focusable          :: Boolean
    , fullWidth          :: Boolean
    , hueLabel           :: String
    , onColorSwatchClick :: ValueHandler ColorFormula
    , saturationLabel    :: String
    , size               :: MantineSize
    | ColorPicking
    )

data ColorFormat
  = ColorFormatHex
  | ColorFormatHexA
  | ColorFormatRGB
  | ColorFormatRGBA
  | ColorFormatHSL
  | ColorFormatHSLA

type ColorFormatImpl = String

instance ToFFI ColorFormat ColorFormatImpl where
  toNative = case _ of
    ColorFormatHex  -> "hex"
    ColorFormatHexA -> "hexa"
    ColorFormatRGB  -> "rgb"
    ColorFormatRGBA -> "rgba"
    ColorFormatHSL  -> "hsl"
    ColorFormatHSLA -> "hsla"

instance FromFFI String ColorFormat where
  fromNative = case _ of
    "hex"  -> ColorFormatHex
    "hexa" -> ColorFormatHexA
    "rgb"  -> ColorFormatRGB
    "rgba" -> ColorFormatRGBA
    "hsl"  -> ColorFormatHSL
    "hsla" -> ColorFormatHSLA
    _      -> ColorFormatHex

newtype ColorFormula = ColorFormula String

type ColorFormulaImpl = String

instance ToFFI ColorFormula ColorFormulaImpl where toNative (ColorFormula cf) = cf

instance FromFFI String ColorFormula where fromNative = ColorFormula

type ColorPickingImpl =
  ( format         :: ColorFormatImpl
  , onChangeEnd    :: ValueHandlerImpl ColorFormulaImpl
  , swatches       :: Array ColorFormulaImpl
  , swatchesPerRow :: Number
  , withPicker     :: Boolean
  | ControlledImpl ColorFormulaImpl
  )

type Props_ColorPickerImpl =
  Props_CommonImpl
    ( alphaLabel         :: String
    , focusable          :: Boolean
    , fullWidth          :: Boolean
    , hueLabel           :: String
    , onColorSwatchClick :: ValueHandlerImpl ColorFormulaImpl
    , saturationLabel    :: String
    , size               :: MantineSizeImpl
    | ColorPickingImpl
    )

alphaSlider
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_AlphaSlider
  => Union attrsImpl attrsImpl_ Props_AlphaSliderImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
alphaSlider = element (unsafeCoerce alphaSliderComponent) <<< toNative

foreign import alphaSliderComponent :: ReactComponent (Record Props_AlphaSliderImpl)

type Props_AlphaSlider     = ( color :: String, value :: Number, onChange :: ValueHandler     Number )
type Props_AlphaSliderImpl = ( color :: String, value :: Number, onChange :: ValueHandlerImpl Number )

hueSlider
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_HueSlider
  => Union attrsImpl attrsImpl_ Props_HueSliderImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
hueSlider = element (unsafeCoerce hueSliderComponent) <<< toNative

foreign import hueSliderComponent :: ReactComponent (Record Props_HueSliderImpl)

type Props_HueSlider     = ( value :: Int,    onChange :: ValueHandler     Int )
type Props_HueSliderImpl = ( value :: Number, onChange :: ValueHandlerImpl Number )
