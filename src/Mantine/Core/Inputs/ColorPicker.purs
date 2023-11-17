module Mantine.Core.Inputs.ColorPicker
  ( colorPicker
  , Props_ColorPicker
  , Props_ColorPickerImpl
  , ColorFormat(..)
  , ColorFormatImpl
  , ColorFormula(..)
  , ColorFormulaImpl

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
