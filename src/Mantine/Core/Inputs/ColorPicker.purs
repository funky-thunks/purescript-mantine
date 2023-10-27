module Mantine.Core.Inputs.ColorPicker
  ( colorPicker
  , ColorPickerProps
  , ColorFormat(..)
  , ColorFormula(..)
  ) where

import Mantine.Core.Prelude

colorPicker :: (ColorPickerProps -> ColorPickerProps) -> JSX
colorPicker = mkComponent colorPickerComponent toNative defaultColorPickerProps

foreign import colorPickerComponent :: ReactComponent ColorPickerPropsImpl

type ColorPickerProps =
  ThemingProps
    ( alphaLabel         :: Maybe String
    , defaultValue       :: Maybe ColorFormula
    , focusable          :: Boolean
    , format             :: ColorFormat
    , fullWidth          :: Boolean
    , hueLabel           :: Maybe String
    , onChange           :: ValueHandler ColorFormula
    , onChangeEnd        :: ValueHandler ColorFormula
    , onColorSwatchClick :: ValueHandler ColorFormula
    , saturationLabel    :: Maybe String
    , size               :: MantineSize
    , swatches           :: Maybe (Array ColorFormula)
    , swatchesPerRow     :: Int
    , value              :: Maybe ColorFormula
    , withPicker         :: Boolean
    )

defaultColorPickerProps :: ColorPickerProps
defaultColorPickerProps =
  defaultThemingProps
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

instance ToFFI ColorFormat String where
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

instance ToFFI ColorFormula String where toNative (ColorFormula cf) = cf

instance FromFFI String ColorFormula where fromNative = ColorFormula

type ColorPickerPropsImpl =
  ThemingPropsImpl
    ( alphaLabel         :: Nullable String
    , defaultValue       :: Nullable String
    , focusable          :: Boolean
    , format             :: String
    , fullWidth          :: Boolean
    , hueLabel           :: Nullable String
    , onChange           :: EffectFn1 String Unit
    , onChangeEnd        :: EffectFn1 String Unit
    , onColorSwatchClick :: EffectFn1 String Unit
    , saturationLabel    :: Nullable String
    , size               :: String
    , swatches           :: Nullable (Array String)
    , swatchesPerRow     :: Number
    , value              :: Nullable String
    , withPicker         :: Boolean
    )
