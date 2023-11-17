module Mantine.Core.Inputs.ColorInput
  ( colorInput
  , ColorInputProps
  , PopoverProps
  ) where

import Mantine.Core.Inputs.ColorPicker (ColorPicking, ColorPickingImpl)
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
    , disallowInput           :: Boolean
    , eyeDropperIcon          :: Optional JSX
    , fixOnBlur               :: Boolean
    , popoverProps            :: PopoverProps
    , withEyeDropper          :: Boolean
    , withPreview             :: Boolean
    | ColorPicking
    )

-- Not supported properties
--   { portalProps :: Omit<PortalProps, "children" | "withinPortal">
--   }

type PopoverProps =
  { dropdownZIndex  :: Optional ZIndex
  , shadow          :: Optional MantineShadow
  , transitionProps :: MantineTransitionProps
  , withinPortal    :: Boolean
  }

defaultColorInputProps :: ColorInputProps
defaultColorInputProps = defaultMantineComponent { swatchesPerRow: 10 }

type ColorInputPropsImpl =
  InputComponentImpl
    ( closeOnColorSwatchClick :: Boolean
    , disallowInput           :: Boolean
    , eyeDropperIcon          :: OptionalImpl JSX
    , fixOnBlur               :: Boolean
    , popoverProps            :: PopoverPropsImpl
    , withEyeDropper          :: Boolean
    , withPreview             :: Boolean
    | ColorPickingImpl
    )

type PopoverPropsImpl =
  { dropdownZIndex  :: OptionalImpl ZIndexImpl
  , shadow          :: OptionalImpl String
  , transitionProps :: MantineTransitionPropsImpl
  , withinPortal    :: Boolean
  }
