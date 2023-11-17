module Mantine.Core.Inputs.PasswordInput
  ( passwordInput
  , PasswordInputProps
  ) where

import Prelude (negate)
import Mantine.Core.Inputs.Input (InputComponent, InputComponentImpl)
import Mantine.Core.Prelude

passwordInput :: (PasswordInputProps -> PasswordInputProps) -> JSX
passwordInput = mkComponent passwordInputComponent passwordInputToImpl defaultMantineComponent_

foreign import passwordInputComponent :: ReactComponent PasswordInputPropsImpl

type PasswordInputProps =
  InputComponent
    ( defaultVisible        :: Optional Boolean
    , onChange              :: InputHandler
    , onVisibilityChange    :: ValueHandler Boolean
    , toggleFocusable       :: Boolean
    , value                 :: Optional String
    , visibilityToggleIcon  :: Optional ({ reveal :: Boolean, size :: Number } -> JSX)
    , visibilityToggleLabel :: Optional String
    , visible               :: Optional Boolean
    )

type PasswordInputPropsImpl =
  InputComponentImpl
    ( defaultVisible              :: OptionalImpl Boolean
    , onChange                    :: InputHandlerImpl
    , onVisibilityChange          :: ValueHandlerImpl Boolean
    , value                       :: OptionalImpl String
    , visibilityToggleButtonProps :: ToggleButtonProps
    , visibilityToggleIcon        :: OptionalImpl ({ reveal :: Boolean, size :: Number } -> JSX)
    , visible                     :: OptionalImpl Boolean
    )

type ToggleButtonProps =
  { toggleTabIndex        :: Number
  , visibilityToggleLabel :: OptionalImpl String
  }

passwordInputToImpl :: PasswordInputProps -> PasswordInputPropsImpl
passwordInputToImpl props =
  let rest = toNative
         <<< delete (Proxy :: Proxy "toggleFocusable")
         <<< delete (Proxy :: Proxy "visibilityToggleIcon")
         <<< delete (Proxy :: Proxy "visibilityToggleLabel")
      visibilityToggleIcon = toOptionalImpl props.visibilityToggleIcon
      visibilityToggleButtonProps =
        { toggleTabIndex: if props.toggleFocusable then 0.0 else -1.0
        , visibilityToggleLabel: toOptionalImpl props.visibilityToggleLabel
        }
   in { visibilityToggleButtonProps, visibilityToggleIcon } `union` rest props
