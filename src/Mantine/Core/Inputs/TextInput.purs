module Mantine.Core.Inputs.TextInput
  ( textInput
  , TextInputProps

  , module Mantine.Core.Inputs.Input
  ) where

import Mantine.Core.Inputs.Input (InputType(..), InputVariant(..), InputWrapperOrder(..))
import Mantine.Core.Prelude

textInput :: (TextInputProps -> TextInputProps) -> JSX
textInput = mkTrivialComponent textInputComponent

foreign import textInputComponent :: ReactComponent TextInputPropsImpl

type TextInputProps =
  ThemingProps
    ( description       :: Maybe JSX
    , disabled          :: Boolean
    , error             :: Maybe JSX
    , icon              :: Maybe JSX
    , iconWidth         :: Maybe Pixels
    , id                :: Maybe String
    , inputContainer    :: Maybe (JSX -> JSX)
    , inputWrapperOrder :: Maybe (Array InputWrapperOrder)
    , label             :: Maybe JSX
    , onChange          :: InputHandler
    , placeholder       :: Maybe String
    , radius            :: Maybe MantineNumberSize
    , required          :: Boolean
    , rightSection      :: Maybe JSX
    , rightSectionWidth :: Maybe Pixels
    , size              :: Maybe MantineSize
    , type              :: Maybe InputType
    , value             :: Maybe String
    , variant           :: InputVariant
    , withAsterisk      :: Boolean
    )

type TextInputPropsImpl =
  ThemingPropsImpl
    ( description       :: Nullable JSX
    , disabled          :: Boolean
    , error             :: Nullable JSX
    , icon              :: Nullable JSX
    , iconWidth         :: Nullable Number
    , id                :: Nullable String
    , inputContainer    :: Nullable (JSX -> JSX)
    , inputWrapperOrder :: Nullable (Array String)
    , label             :: Nullable JSX
    , onChange          :: EffectFn1 SyntheticEvent Unit
    , placeholder       :: Nullable String
    , radius            :: Nullable MantineNumberSizeImpl
    , required          :: Boolean
    , rightSection      :: Nullable JSX
    , rightSectionWidth :: Nullable Number
    , size              :: Nullable String
    , type              :: Nullable String
    , value             :: Nullable String
    , variant           :: String
    , withAsterisk      :: Boolean
    )
