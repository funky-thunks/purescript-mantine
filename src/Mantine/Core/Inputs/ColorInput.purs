module Mantine.Core.Inputs.ColorInput
  ( colorInput
  , ColorInputProps
  , PopoverProps

  , module Mantine.Core.Inputs.ColorPicker
  ) where

import Mantine.Core.Inputs.ColorPicker (ColorFormat, ColorFormula)
import Mantine.Core.Inputs.Input (InputComponent, InputComponentImpl)
import Mantine.Core.Prelude

colorInput :: (ColorInputProps -> ColorInputProps) -> JSX
colorInput = mkComponentWithDefault colorInputComponent defaultColorInputProps

foreign import colorInputComponent :: ReactComponent ColorInputPropsImpl

-- Not supported properties
--   { eyeDropperButtonProps :: Record<string, any>
--   }

type ColorInputProps =
  InputComponent
    ( closeOnColorSwatchClick :: Boolean
    , defaultValue            :: Maybe ColorFormula
    , disallowInput           :: Boolean
    , eyeDropperIcon          :: Maybe JSX
    , fixOnBlur               :: Boolean
    , format                  :: ColorFormat
    , onChange                :: ValueHandler ColorFormula
    , onChangeEnd             :: ValueHandler ColorFormula
    , popoverProps            :: PopoverProps
    , swatches                :: Maybe (Array ColorFormula)
    , swatchesPerRow          :: Int
    , value                   :: Maybe ColorFormula
    , withEyeDropper          :: Boolean
    , withPicker              :: Boolean
    , withPreview             :: Boolean
    )

-- Not supported properties
--   { portalProps :: Omit<PortalProps, "children" | "withinPortal">
--   }

type PopoverProps =
  { dropdownZIndex  :: Maybe Number
  , shadow          :: Maybe MantineShadow
  , transitionProps :: MantineTransitionProps
  , withinPortal    :: Boolean
  }

defaultColorInputProps :: ColorInputProps
defaultColorInputProps = defaultThemingProps { swatchesPerRow: 10 }

type ColorInputPropsImpl =
  InputComponentImpl
    ( closeOnColorSwatchClick :: Boolean
    , defaultValue            :: Nullable String
    , disallowInput           :: Boolean
    , eyeDropperIcon          :: Nullable JSX
    , fixOnBlur               :: Boolean
    , format                  :: String
    , onChange                :: EffectFn1 String Unit
    , onChangeEnd             :: EffectFn1 String Unit
    , popoverProps            :: PopoverPropsImpl
    , swatches                :: Nullable (Array String)
    , swatchesPerRow          :: Number
    , value                   :: Nullable String
    , withEyeDropper          :: Boolean
    , withPicker              :: Boolean
    , withPreview             :: Boolean
    )

type PopoverPropsImpl =
  { dropdownZIndex  :: Nullable Number
  , shadow          :: Nullable String
  , transitionProps :: MantineTransitionPropsImpl
  , withinPortal    :: Boolean
  }
