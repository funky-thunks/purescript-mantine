module Mantine.Core.Inputs.ColorInput
  ( colorInput
  , ColorInputProps

  , module Mantine.Core.Inputs.ColorPicker
  , module Mantine.Core.Inputs.Input
  ) where

import Mantine.Core.Inputs.ColorPicker (ColorFormat, ColorFormula)
import Mantine.Core.Inputs.Input (InputVariant(..), InputWrapperOrder(..))
import Mantine.Core.Prelude

colorInput :: (ColorInputProps -> ColorInputProps) -> JSX
colorInput = mkComponentWithDefault colorInputComponent defaultColorInputProps

foreign import colorInputComponent :: ReactComponent ColorInputPropsImpl

-- Not supported properties
--   { descriptionProps         :: Record<string, any>
--   , errorProps               :: Record<string, any>
--   , labelProps               :: Record<string, any>
--   , portalProps              :: Omit<PortalProps, "children" | "withinPortal">
--   , rightSectionProps        :: Record<string, any>
--   , wrapperProps             :: Record<string, any>
--   }

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
    , eyeDropperLabel          :: Maybe String
    , fixOnBlur                :: Boolean
    , format                   :: ColorFormat
    , icon                     :: Maybe JSX
    , iconWidth                :: Maybe Pixels
    , inputContainer           :: Maybe (JSX -> JSX)
    , inputWrapperOrder        :: Maybe (Array InputWrapperOrder)
    , label                    :: Maybe JSX
    , onChange                 :: ValueHandler ColorFormula
    , onChangeEnd              :: ValueHandler ColorFormula
    , radius                   :: Maybe MantineNumberSize
    , required                 :: Boolean
    , rightSection             :: Maybe JSX
    , rightSectionWidth        :: Maybe Pixels
    , shadow                   :: Maybe MantineShadow
    , size                     :: MantineSize
    , swatches                 :: Maybe (Array ColorFormula)
    , swatchesPerRow           :: Int
    , transitionProps          :: MantineTransitionProps
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
    , eyeDropperLabel          :: Nullable String
    , fixOnBlur                :: Boolean
    , format                   :: String
    , icon                     :: Nullable JSX
    , iconWidth                :: Nullable Number
    , inputContainer           :: Nullable (JSX -> JSX)
    , inputWrapperOrder        :: Nullable (Array String)
    , label                    :: Nullable JSX
    , onChange                 :: EffectFn1 String Unit
    , onChangeEnd              :: EffectFn1 String Unit
    , radius                   :: Nullable MantineNumberSizeImpl
    , required                 :: Boolean
    , rightSection             :: Nullable JSX
    , rightSectionWidth        :: Nullable Number
    , shadow                   :: Nullable String
    , size                     :: String
    , swatches                 :: Nullable (Array String)
    , swatchesPerRow           :: Number
    , transitionProps          :: MantineTransitionPropsImpl
    , value                    :: Nullable String
    , variant                  :: String
    , withAsterisk             :: Boolean
    , withEyeDropper           :: Boolean
    , withPicker               :: Boolean
    , withPreview              :: Boolean
    , withinPortal             :: Boolean
    )
