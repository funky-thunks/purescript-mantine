module Mantine.Core.Inputs.ColorInput
  ( colorInput
  , Props_ColorInput
  , Props_ColorInputImpl
  , PopoverProps
  , PopoverPropsImpl
  ) where

import Mantine.Core.Inputs.ColorPicker (ColorPicking, ColorPickingImpl)
import Mantine.Core.Inputs.Input (Props_InputComponent, Props_InputComponentImpl)
import Mantine.Core.Prelude

colorInput
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_ColorInput
  => Union attrsImpl attrsImpl_ Props_ColorInputImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
colorInput = element (unsafeCoerce colorInputComponent) <<< toNative

foreign import colorInputComponent :: ReactComponent (Record Props_ColorInputImpl)

-- Not supported properties
--   { eyeDropperButtonProps :: Record<string, any>
--   }

type Props_ColorInput =
  Props_InputComponent
    ( closeOnColorSwatchClick :: Boolean
    , disallowInput           :: Boolean
    , eyeDropperIcon          :: JSX
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

type Props_ColorInputImpl =
  Props_InputComponentImpl
    ( closeOnColorSwatchClick :: Boolean
    , disallowInput           :: Boolean
    , eyeDropperIcon          :: JSX
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
