module Mantine.Core.Inputs.ColorPicker
  ( colorPicker
  , ColorPickerProps
  , ColorFormat(..)
  , ColorFormatImpl
  , ColorFormula(..)
  , ColorFormulaImpl

  , ColorPicking
  , ColorPickingImpl
  ) where

import Mantine.Core.Prelude

colorPicker :: (ColorPickerProps -> ColorPickerProps) -> JSX
colorPicker = mkComponentWithDefault colorPickerComponent defaultColorPickerProps

foreign import colorPickerComponent :: ReactComponent ColorPickerPropsImpl

type ColorPicking =
  ( format         :: ColorFormat
  , onChangeEnd    :: ValueHandler ColorFormula
  , swatches       :: Maybe (Array ColorFormula)
  , swatchesPerRow :: Int
  , withPicker     :: Boolean
  | Controlled ColorFormula
  )

type ColorPickerProps =
  MantineComponent
    ( alphaLabel         :: Maybe String
    , focusable          :: Boolean
    , fullWidth          :: Boolean
    , hueLabel           :: Maybe String
    , onColorSwatchClick :: ValueHandler ColorFormula
    , saturationLabel    :: Maybe String
    , size               :: MantineSize
    | ColorPicking
    )

defaultColorPickerProps :: ColorPickerProps
defaultColorPickerProps =
  defaultMantineComponent
    { focusable: true
    , size: Small
    , swatchesPerRow: 10
    }

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
    _      -> defaultValue

instance DefaultValue ColorFormat where defaultValue = ColorFormatHex

newtype ColorFormula = ColorFormula String

type ColorFormulaImpl = String

instance ToFFI ColorFormula ColorFormulaImpl where toNative (ColorFormula cf) = cf

instance FromFFI String ColorFormula where fromNative = ColorFormula

type ColorPickingImpl =
  ( format         :: ColorFormatImpl
  , onChangeEnd    :: ValueHandlerImpl ColorFormulaImpl
  , swatches       :: Nullable (Array ColorFormulaImpl)
  , swatchesPerRow :: Number
  , withPicker     :: Boolean
  | ControlledImpl ColorFormulaImpl
  )

type ColorPickerPropsImpl =
  MantineComponentImpl
    ( alphaLabel         :: Nullable String
    , focusable          :: Boolean
    , fullWidth          :: Boolean
    , hueLabel           :: Nullable String
    , onColorSwatchClick :: ValueHandlerImpl ColorFormulaImpl
    , saturationLabel    :: Nullable String
    , size               :: MantineSizeImpl
    | ColorPickingImpl
    )
