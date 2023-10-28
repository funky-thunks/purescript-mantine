module Mantine.Core.Inputs.ColorInput
  ( colorInput
  , ColorInputProps

  , module Mantine.Core.Inputs.ColorPicker
  , module Mantine.Core.Inputs.Input
  ) where

import Mantine.Core.Prelude
import Mantine.Core.Inputs.ColorPicker (ColorFormat, ColorFormula)
import Mantine.Core.Inputs.Input (InputVariant(..))

colorInput :: (ColorInputProps -> ColorInputProps) -> JSX
colorInput = mkComponentWithDefault colorInputComponent defaultColorInputProps

foreign import colorInputComponent :: ReactComponent ColorInputPropsImpl

-- Not supported properties
-- descriptionProps         :: Record<string, any>
-- errorProps               :: Record<string, any>
-- inputContainer           :: (children: ReactNode) => ReactNode
-- inputWrapperOrder        :: ("input" | "label" | "error" | "description")[]
-- labelProps               :: Record<string, any>
-- rightSectionProps        :: Record<string, any>
-- shadow                   :: MantineShadow
-- wrapperProps             :: Record<string, any>

type ColorInputProps =
  ThemingProps
    ( closeOnColorSwatchClick  :: Boolean
    , defaultValue             :: Maybe ColorFormula
    , description              :: Maybe JSX
    , disabled                 :: Boolean
    , disallowInput            :: Boolean
    , dropdownZIndex           :: Maybe Number
    , error                    :: Maybe JSX
    , eyeDropperIcon           :: Maybe JSX
    , fixOnBlur                :: Boolean
    , format                   :: ColorFormat
    , icon                     :: Maybe JSX
    , iconWidth                :: Maybe Pixels
    , label                    :: Maybe JSX
    , onChange                 :: ValueHandler ColorFormula
    , onChangeEnd              :: ValueHandler ColorFormula
    , radius                   :: Maybe MantineNumberSize
    , required                 :: Boolean
    , rightSection             :: Maybe JSX
    , rightSectionWidth        :: Maybe Pixels
    , size                     :: MantineSize
    , swatches                 :: Maybe (Array ColorFormula)
    , swatchesPerRow           :: Int
    , transition               :: Maybe MantineTransition
    , transitionDuration       :: Maybe Milliseconds
    , transitionTimingFunction :: Maybe MantineTransitionTimingFunction
    , value                    :: Maybe ColorFormula
    , variant                  :: InputVariant
    , withAsterisk             :: Boolean
    , withEyeDropper           :: Boolean
    , withPicker               :: Boolean
    , withPreview              :: Boolean
    , withinPortal             :: Boolean
    )

defaultColorInputProps :: ColorInputProps
defaultColorInputProps =
  defaultThemingProps
    { size: Small
    , swatchesPerRow: 10
    }

type ColorInputPropsImpl =
  ThemingPropsImpl
    ( closeOnColorSwatchClick  :: Boolean
    , defaultValue             :: Nullable String
    , description              :: Nullable JSX
    , disabled                 :: Boolean
    , disallowInput            :: Boolean
    , dropdownZIndex           :: Nullable Number
    , error                    :: Nullable JSX
    , eyeDropperIcon           :: Nullable JSX
    , fixOnBlur                :: Boolean
    , format                   :: String
    , icon                     :: Nullable JSX
    , iconWidth                :: Nullable Number
    , label                    :: Nullable JSX
    , onChange                 :: EffectFn1 String Unit
    , onChangeEnd              :: EffectFn1 String Unit
    , radius                   :: Nullable MantineNumberSizeImpl
    , required                 :: Boolean
    , rightSection             :: Nullable JSX
    , rightSectionWidth        :: Nullable Number
    , size                     :: String
    , swatches                 :: Nullable (Array String)
    , swatchesPerRow           :: Number
    , transition               :: Nullable String
    , transitionDuration       :: Nullable Number
    , transitionTimingFunction :: Nullable String
    , value                    :: Nullable String
    , variant                  :: String
    , withAsterisk             :: Boolean
    , withEyeDropper           :: Boolean
    , withPicker               :: Boolean
    , withPreview              :: Boolean
    , withinPortal             :: Boolean
    )
