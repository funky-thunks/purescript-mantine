module Mantine.Core.Inputs.PasswordInput
  ( passwordInput
  , PasswordInputProps

  , module Mantine.Core.Inputs.Input
  ) where

import Prelude (negate)
import Mantine.Core.Prelude
import Mantine.Core.Inputs.Input (InputType(..), InputVariant(..), InputWrapperOrder(..))

passwordInput :: (PasswordInputProps -> PasswordInputProps) -> JSX
passwordInput = mkComponent passwordInputComponent passwordInputToImpl defaultThemingProps_

foreign import passwordInputComponent :: ReactComponent PasswordInputPropsImpl

type PasswordInputProps =
  ThemingProps
    ( defaultVisible        :: Maybe Boolean
    , description           :: Maybe JSX
    , disabled              :: Boolean
    , error                 :: Maybe JSX
    , icon                  :: Maybe JSX
    , iconWidth             :: Maybe Pixels
    , id                    :: Maybe String
    , inputContainer        :: Maybe (JSX -> JSX)
    , inputWrapperOrder     :: Maybe (Array InputWrapperOrder)
    , label                 :: Maybe JSX
    , onChange              :: InputHandler
    , onVisibilityChange    :: ValueHandler Boolean
    , radius                :: Maybe MantineNumberSize
    , required              :: Boolean
    , size                  :: Maybe MantineSize
    , toggleFocusable       :: Boolean
    , type                  :: Maybe InputType
    , value                 :: Maybe String
    , variant               :: InputVariant
    , visibilityToggleIcon  :: Maybe ({ reveal :: Boolean, size :: Number } -> JSX)
    , visibilityToggleLabel :: Maybe String
    , visible               :: Maybe Boolean
    , withAsterisk          :: Boolean
    )

type PasswordInputPropsImpl =
  ThemingPropsImpl
    ( defaultVisible        :: Nullable Boolean
    , description           :: Nullable JSX
    , disabled              :: Boolean
    , error                 :: Nullable JSX
    , icon                  :: Nullable JSX
    , iconWidth             :: Nullable Number
    , id                    :: Nullable String
    , inputContainer        :: Nullable (JSX -> JSX)
    , inputWrapperOrder     :: Nullable (Array String)
    , label                 :: Nullable JSX
    , onChange              :: EffectFn1 SyntheticEvent Unit
    , onVisibilityChange    :: EffectFn1 Boolean Unit
    , radius                :: Nullable MantineNumberSizeImpl
    , required              :: Boolean
    , size                  :: Nullable String
    , toggleTabIndex        :: Number
    , type                  :: Nullable String
    , value                 :: Nullable String
    , variant               :: String
    , visibilityToggleIcon  :: Nullable ({ reveal :: Boolean, size :: Number } -> JSX)
    , visibilityToggleLabel :: Nullable String
    , visible               :: Nullable Boolean
    , withAsterisk          :: Boolean
    )

passwordInputToImpl :: PasswordInputProps -> PasswordInputPropsImpl
passwordInputToImpl props =
  let rest = toNative
         <<< delete (Proxy :: Proxy "inputContainer")
         <<< delete (Proxy :: Proxy "toggleFocusable")
         <<< delete (Proxy :: Proxy "visibilityToggleIcon")
   in { inputContainer: toNullable props.inputContainer
      , toggleTabIndex: if props.toggleFocusable then 0.0 else -1.0
      , visibilityToggleIcon: toNullable props.visibilityToggleIcon
      } `union` rest props
