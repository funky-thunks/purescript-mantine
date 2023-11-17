module Mantine.Core.Inputs.PasswordInput
  ( passwordInput
  , Props_PasswordInput
  , Props_PasswordInputImpl
  , ToggleOptions(..)
  , ToggleButtonProps
  ) where

import Prelude (negate)
import Mantine.Core.Inputs.Input (Props_InputComponent, Props_InputComponentImpl)
import Mantine.Core.Prelude

passwordInput
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_PasswordInput
  => Union attrsImpl attrsImpl_ Props_PasswordInputImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
passwordInput = element (unsafeCoerce passwordInputComponent) <<< toNative

foreign import passwordInputComponent :: ReactComponent (Record Props_PasswordInputImpl)

type Props_PasswordInput =
  Props_InputComponent
    ( defaultVisible              :: Boolean
    , onChange                    :: InputHandler
    , onVisibilityChange          :: ValueHandler Boolean
    , value                       :: String
    , visibilityToggleButtonProps :: ToggleOptions
    , visibilityToggleIcon        :: { reveal :: Boolean, size :: Number } -> JSX
    , visible                     :: Boolean
    )

type Props_PasswordInputImpl =
  Props_InputComponentImpl
    ( defaultVisible              :: Boolean
    , onChange                    :: InputHandlerImpl
    , onVisibilityChange          :: ValueHandlerImpl Boolean
    , value                       :: String
    , visibilityToggleButtonProps :: ToggleButtonProps
    , visibilityToggleIcon        :: { reveal :: Boolean, size :: Number } -> JSX
    , visible                     :: Boolean
    )

newtype ToggleOptions = ToggleOptions { focusable :: Boolean, label :: String }

type ToggleButtonProps =
  { toggleTabIndex        :: Number
  , visibilityToggleLabel :: String
  }

instance ToFFI ToggleOptions ToggleButtonProps where
  toNative (ToggleOptions { focusable, label }) =
    { toggleTabIndex:        if focusable then 0.0 else -1.0
    , visibilityToggleLabel: label
    }
