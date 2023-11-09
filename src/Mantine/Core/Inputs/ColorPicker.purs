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
  ( defaultValue   :: Maybe ColorFormula
  , format         :: ColorFormat
  , onChange       :: ValueHandler ColorFormula
  , onChangeEnd    :: ValueHandler ColorFormula
  , swatches       :: Maybe (Array ColorFormula)
  , swatchesPerRow :: Int
  , value          :: Maybe ColorFormula
  , withPicker     :: Boolean
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
  ( defaultValue   :: Nullable ColorFormulaImpl
  , format         :: ColorFormatImpl
  , onChange       :: ValueHandlerImpl ColorFormulaImpl
  , onChangeEnd    :: ValueHandlerImpl ColorFormulaImpl
  , swatches       :: Nullable (Array ColorFormulaImpl)
  , swatchesPerRow :: Number
  , value          :: Nullable ColorFormulaImpl
  , withPicker     :: Boolean
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
