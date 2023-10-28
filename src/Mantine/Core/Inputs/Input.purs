module Mantine.Core.Inputs.Input
  ( InputVariant(..)
  , InputWrapperOrder(..)

  , input
  , InputProps

  , inputWrapper
  , InputWrapperProps
  , InputWrapperElement(..)

  , inputLabel
  , InputLabelProps

  , inputDescription
  , InputDescriptionProps

  , inputError
  , InputErrorProps
  ) where

import Mantine.Core.Prelude

data InputVariant
  = InputVariantDefault
  | InputVariantUnstyled
  | InputVariantFilled

instance DefaultValue InputVariant where defaultValue = InputVariantDefault

instance ToFFI InputVariant String where
  toNative = case _ of
    InputVariantDefault  -> "default"
    InputVariantUnstyled -> "unstyled"
    InputVariantFilled   -> "filled"

data InputWrapperOrder
  = InputWrapperOrderInput
  | InputWrapperOrderLabel
  | InputWrapperOrderError
  | InputWrapperOrderDescription

instance ToFFI InputWrapperOrder String where
  toNative = case _ of
    InputWrapperOrderInput       -> "input"
    InputWrapperOrderLabel       -> "label"
    InputWrapperOrderError       -> "error"
    InputWrapperOrderDescription -> "description"

input :: (InputProps -> InputProps) -> JSX
input = mkTrivialComponent inputComponent

foreign import inputComponent :: ReactComponent InputPropsImpl

type InputProps =
  ThemingProps
    ( disabled          :: Boolean
    , icon              :: Maybe JSX
    , iconWidth         :: Maybe Pixels
    , invalid           :: Boolean
    , multiline         :: Boolean
    , pointer           :: Boolean
    , radius            :: Maybe MantineNumberSize
    , required          :: Boolean
    , rightSection      :: Maybe JSX
    , rightSectionWidth :: Maybe Pixels
    , size              :: Maybe MantineSize
    , variant           :: InputVariant
    )

type InputPropsImpl =
  ThemingPropsImpl
    ( disabled          :: Boolean
    , icon              :: Nullable JSX
    , iconWidth         :: Nullable Number
    , invalid           :: Boolean
    , multiline         :: Boolean
    , pointer           :: Boolean
    , radius            :: Nullable MantineNumberSizeImpl
    , required          :: Boolean
    , rightSection      :: Nullable JSX
    , rightSectionWidth :: Nullable Number
    , size              :: Nullable String
    , variant           :: String
    )

inputWrapper :: (InputWrapperProps -> InputWrapperProps) -> JSX
inputWrapper = mkComponent inputWrapperComponent inputWrapperPropsToImpl defaultThemingProps_

foreign import inputWrapperComponent :: ReactComponent InputWrapperPropsImpl

type InputWrapperProps =
  ThemingProps
    ( children          :: Array JSX
    , description       :: Maybe JSX
    , error             :: Maybe JSX
    , id                :: Maybe String
    , inputContainer    :: Maybe (JSX -> JSX)
    , inputWrapperOrder :: Maybe (Array InputWrapperOrder)
    , label             :: Maybe JSX
    , labelElement      :: Maybe InputWrapperElement
    , required          :: Boolean
    , size              :: Maybe MantineSize
    , withAsterisk      :: Boolean
    )

data InputWrapperElement = InputWrapperDiv | InputWrapperLabel

instance ToFFI InputWrapperElement String where
  toNative = case _ of
    InputWrapperDiv   -> "div"
    InputWrapperLabel -> "label"

type InputWrapperPropsImpl =
  ThemingPropsImpl
    ( children          :: Array JSX
    , description       :: Nullable JSX
    , error             :: Nullable JSX
    , id                :: Nullable String
    , inputContainer    :: Nullable (JSX -> JSX)
    , inputWrapperOrder :: Nullable (Array String)
    , label             :: Nullable JSX
    , labelElement      :: Nullable String
    , required          :: Boolean
    , size              :: Nullable String
    , withAsterisk      :: Boolean
    )

inputWrapperPropsToImpl :: InputWrapperProps -> InputWrapperPropsImpl
inputWrapperPropsToImpl props =
  let rest = toNative
         <<< delete (Proxy :: Proxy "inputContainer")
   in { inputContainer: toNullable props.inputContainer } `union` rest props

inputLabel :: (InputLabelProps -> InputLabelProps) -> JSX
inputLabel = mkTrivialComponent inputLabelComponent

foreign import inputLabelComponent :: ReactComponent InputLabelPropsImpl

type InputLabelProps =
  ThemingProps
    ( children     :: Array JSX
    , labelElement :: Maybe InputWrapperElement
    , required     :: Boolean
    , size         :: Maybe MantineSize
    )

type InputLabelPropsImpl =
  ThemingPropsImpl
    ( children     :: Array JSX
    , labelElement :: Nullable String
    , required     :: Boolean
    , size         :: Nullable String
    )

inputDescription :: (InputDescriptionProps -> InputDescriptionProps) -> JSX
inputDescription = mkTrivialComponent inputDescriptionComponent

foreign import inputDescriptionComponent :: ReactComponent InputDescriptionPropsImpl

type InputDescriptionProps =
  ThemingProps
    ( children :: Array JSX
    , size     :: Maybe MantineSize
    )

type InputDescriptionPropsImpl =
  ThemingPropsImpl
    ( children :: Array JSX
    , size     :: Nullable String
    )

inputError :: (InputErrorProps -> InputErrorProps) -> JSX
inputError = mkTrivialComponent inputErrorComponent

foreign import inputErrorComponent :: ReactComponent InputErrorPropsImpl

type InputErrorProps =
  ThemingProps
    ( children :: Array JSX
    , size     :: Maybe MantineSize
    )

type InputErrorPropsImpl =
  ThemingPropsImpl
    ( children :: Array JSX
    , size     :: Nullable String
    )
