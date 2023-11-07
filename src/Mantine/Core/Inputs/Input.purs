module Mantine.Core.Inputs.Input
  ( InputVariant(..)
  , InputWrapperOrder(..)
  , InputType(..)

  , input
  , InputProps
  , InputPropsRow
  , InputPropsRow_
  , InputGroupPropsRow
  , InputGroupPropsRow_
  , InputBasePropsRow
  , InputBasePropsRow_

  , InputPropsRowImpl
  , InputPropsRowImpl_
  , InputGroupPropsRowImpl
  , InputGroupPropsRowImpl_
  , InputBasePropsRowImpl
  , InputBasePropsRowImpl_

  , inputWrapper
  , InputWrapperProps
  , InputWrapperElement(..)

  , inputLabel
  , InputLabelProps

  , inputDescription
  , InputDescriptionProps

  , inputError
  , InputErrorProps

  , InputComponent
  , InputComponentImpl
  , InputGroupComponent
  , InputGroupComponentImpl
  , WithInputContainer
  , WithInputContainerImpl
  ) where

import Mantine.Core.Prelude

data InputVariant
  = InputVariantDefault
  | InputVariantUnstyled
  | InputVariantFilled

instance DefaultValue InputVariant where defaultValue = InputVariantDefault

instance ToFFI InputVariant (Nullable String) where
  toNative = toNative <<< case _ of
    InputVariantDefault  -> Nothing
    InputVariantUnstyled -> Just "unstyled"
    InputVariantFilled   -> Just "filled"

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

data InputType
  = InputTypeButton
  | InputTypeCheckbox
  | InputTypeColor
  | InputTypeDate
  | InputTypeDatetimeLocal
  | InputTypeEmail
  | InputTypeFile
  | InputTypeHidden
  | InputTypeImage
  | InputTypeMonth
  | InputTypeNumber
  | InputTypePassword
  | InputTypeRadio
  | InputTypeRange
  | InputTypeReset
  | InputTypeSearch
  | InputTypeSubmit
  | InputTypeTel
  | InputTypeText
  | InputTypeTime
  | InputTypeUrl
  | InputTypeWeek

instance ToFFI InputType String where
  toNative = case _ of
    InputTypeButton        -> "button"
    InputTypeCheckbox      -> "checkbox"
    InputTypeColor         -> "color"
    InputTypeDate          -> "date"
    InputTypeDatetimeLocal -> "datetime-local"
    InputTypeEmail         -> "email"
    InputTypeFile          -> "file"
    InputTypeHidden        -> "hidden"
    InputTypeImage         -> "image"
    InputTypeMonth         -> "month"
    InputTypeNumber        -> "number"
    InputTypePassword      -> "password"
    InputTypeRadio         -> "radio"
    InputTypeRange         -> "range"
    InputTypeReset         -> "reset"
    InputTypeSearch        -> "search"
    InputTypeSubmit        -> "submit"
    InputTypeTel           -> "tel"
    InputTypeText          -> "text"
    InputTypeTime          -> "time"
    InputTypeUrl           -> "url"
    InputTypeWeek          -> "week"

input :: (InputProps -> InputProps) -> JSX
input = mkTrivialComponent inputComponent

foreign import inputComponent :: ReactComponent InputPropsImpl

type InputProps = ThemingProps InputPropsRow

-- Not supported properties
--   { descriptionProps  :: Record<string, any>
--   , errorProps        :: Record<string, any>
--   , labelProps        :: Record<string, any>
--   , leftSectionProps  :: Record<string, any>
--   , rightSectionProps :: Record<string, any>
--   , wrapperProps      :: Record<string, any>
--   }

type InputPropsRow = InputPropsRow_ ()

type InputPropsRow_ rest =
  ( disabled                  :: Boolean
  , leftSection               :: Maybe JSX
  , leftSectionPointerEvents  :: Maybe PointerEvents
  , leftSectionWidth          :: Maybe Pixels
  , multiline                 :: Boolean
  , pointer                   :: Boolean
  , radius                    :: Maybe MantineNumberSize
  , rightSection              :: Maybe JSX
  , rightSectionPointerEvents :: Maybe PointerEvents
  , rightSectionWidth         :: Maybe Pixels
  , withAria                  :: Boolean
  , withErrorStyles           :: Maybe Boolean
  | InputBasePropsRow_ rest
  )

type InputGroupPropsRow items = InputGroupPropsRow_ items ()
type InputGroupPropsRow_ items rest =
  ( children     :: Array JSX
  , defaultValue :: Maybe items
  , labelElement :: Maybe InputWrapperElement
  , onChange     :: ValueHandler items
  , value        :: Maybe items
  | InputBasePropsRow_ rest
  )

type InputBasePropsRow = InputBasePropsRow_ ()
type InputBasePropsRow_ rest =
  ( description       :: Maybe JSX
  , error             :: Maybe JSX
  , id                :: Maybe String
  , inputWrapperOrder :: Maybe (Array InputWrapperOrder)
  , label             :: Maybe JSX
  , placeholder       :: Maybe String
  , required          :: Boolean
  , size              :: Maybe MantineSize
  , variant           :: InputVariant
  , withAsterisk      :: Boolean
  | rest
  )

type InputPropsImpl = ThemingPropsImpl InputPropsRowImpl

type InputPropsRowImpl = InputPropsRowImpl_ ()
type InputPropsRowImpl_ rest =
  ( disabled                  :: Boolean
  , leftSection               :: Nullable JSX
  , leftSectionPointerEvents  :: Nullable String
  , leftSectionWidth          :: Nullable Number
  , multiline                 :: Boolean
  , pointer                   :: Boolean
  , radius                    :: Nullable MantineNumberSizeImpl
  , rightSection              :: Nullable JSX
  , rightSectionPointerEvents :: Nullable String
  , rightSectionWidth         :: Nullable Number
  , withAria                  :: Boolean
  , withErrorStyles           :: Nullable Boolean
  | InputBasePropsRowImpl_ rest
  )

type InputGroupPropsRowImpl items = InputGroupPropsRowImpl_ items ()
type InputGroupPropsRowImpl_ items rest =
  ( children     :: Array JSX
  , defaultValue :: Nullable items
  , labelElement :: Nullable String
  , onChange     :: EffectFn1 items Unit
  , value        :: Nullable items
  | InputBasePropsRowImpl_ rest
  )

type InputBasePropsRowImpl = InputBasePropsRowImpl_ ()
type InputBasePropsRowImpl_ rest =
  ( description       :: Nullable JSX
  , error             :: Nullable JSX
  , id                :: Nullable String
  , inputWrapperOrder :: Nullable (Array String)
  , label             :: Nullable JSX
  , placeholder       :: Nullable String
  , required          :: Boolean
  , size              :: Nullable String
  , variant           :: Nullable String
  , withAsterisk      :: Boolean
  | rest
  )

inputWrapper :: (InputWrapperProps -> InputWrapperProps) -> JSX
inputWrapper = mkTrivialComponent inputWrapperComponent

foreign import inputWrapperComponent :: ReactComponent InputWrapperPropsImpl

-- Not supported properties
--   { descriptionProps :: Record<string, any>
--   , errorProps       :: Record<string, any>
--   , labelProps       :: Record<string, any>
--   }

type InputWrapperProps =
  ThemingProps (
    WithInputContainer
      ( children          :: Array JSX
      , description       :: Maybe JSX
      , error             :: Maybe JSX
      , id                :: Maybe String
      , inputWrapperOrder :: Maybe (Array InputWrapperOrder)
      , label             :: Maybe JSX
      , labelElement      :: Maybe InputWrapperElement
      , required          :: Boolean
      , size              :: Maybe MantineSize
      , withAsterisk      :: Boolean
      )
  )

data InputWrapperElement = InputWrapperDiv | InputWrapperLabel

instance ToFFI InputWrapperElement String where
  toNative = case _ of
    InputWrapperDiv   -> "div"
    InputWrapperLabel -> "label"

type InputWrapperPropsImpl =
  ThemingPropsImpl (
    WithInputContainerImpl
      ( children          :: Array JSX
      , description       :: Nullable JSX
      , error             :: Nullable JSX
      , id                :: Nullable String
      , inputWrapperOrder :: Nullable (Array String)
      , label             :: Nullable JSX
      , labelElement      :: Nullable String
      , required          :: Boolean
      , size              :: Nullable String
      , withAsterisk      :: Boolean
      )
  )

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

type WithInputContainer rest =
  ( inputContainer :: Maybe (JSX -> JSX)
  | rest
  )

type WithInputContainerImpl rest =
  ( inputContainer :: Nullable (JSX -> JSX)
  | rest
  )

type InputComponent rest =
  ThemingProps (WithInputContainer + InputPropsRow_ rest)

type InputComponentImpl rest =
  ThemingPropsImpl (WithInputContainerImpl + InputPropsRowImpl_ rest)

type InputGroupComponent items rest =
  ThemingProps (WithInputContainer + InputGroupPropsRow_ items rest)

type InputGroupComponentImpl items rest =
  ThemingPropsImpl (WithInputContainerImpl + InputGroupPropsRowImpl_ items rest)
