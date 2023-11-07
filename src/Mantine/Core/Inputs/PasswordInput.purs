module Mantine.Core.Inputs.PasswordInput
  ( passwordInput
  , PasswordInputProps
  ) where

import Prelude (negate)
import Mantine.Core.Inputs.Input (InputComponent, InputComponentImpl)
import Mantine.Core.Prelude

passwordInput :: (PasswordInputProps -> PasswordInputProps) -> JSX
passwordInput = mkComponent passwordInputComponent passwordInputToImpl defaultThemingProps_

foreign import passwordInputComponent :: ReactComponent PasswordInputPropsImpl

type PasswordInputProps =
  InputComponent
    ( defaultVisible        :: Maybe Boolean
    , onChange              :: InputHandler
    , onVisibilityChange    :: ValueHandler Boolean
    , toggleFocusable       :: Boolean
    , value                 :: Maybe String
    , visibilityToggleIcon  :: Maybe ({ reveal :: Boolean, size :: Number } -> JSX)
    , visibilityToggleLabel :: Maybe String
    , visible               :: Maybe Boolean
    )

type PasswordInputPropsImpl =
  InputComponentImpl
    ( defaultVisible              :: Nullable Boolean
    , onChange                    :: EffectFn1 SyntheticEvent Unit
    , onVisibilityChange          :: EffectFn1 Boolean Unit
    , value                       :: Nullable String
    , visibilityToggleButtonProps :: ToggleButtonProps
    , visibilityToggleIcon        :: Nullable ({ reveal :: Boolean, size :: Number } -> JSX)
    , visible                     :: Nullable Boolean
    )

type ToggleButtonProps =
  { toggleTabIndex        :: Number
  , visibilityToggleLabel :: Nullable String
  }

passwordInputToImpl :: PasswordInputProps -> PasswordInputPropsImpl
passwordInputToImpl props =
  let rest = toNative
         <<< delete (Proxy :: Proxy "toggleFocusable")
         <<< delete (Proxy :: Proxy "visibilityToggleIcon")
         <<< delete (Proxy :: Proxy "visibilityToggleLabel")
      visibilityToggleIcon = toNullable props.visibilityToggleIcon
      visibilityToggleButtonProps =
        { toggleTabIndex: if props.toggleFocusable then 0.0 else -1.0
        , visibilityToggleLabel: toNullable props.visibilityToggleLabel
        }
   in { visibilityToggleButtonProps, visibilityToggleIcon } `union` rest props
