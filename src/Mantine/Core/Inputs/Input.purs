module Mantine.Core.Inputs.Input
  ( InputVariant(..)
  , InputVariantImpl
  , InputWrapperOrder(..)
  , InputWrapperOrderImpl
  , InputType(..)
  , InputTypeImpl

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
  , InputWrapperElementImpl

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

type InputVariantImpl = OptionalImpl String

instance ToFFI InputVariant InputVariantImpl where
  toNative = toNative <<< Optional <<< case _ of
    InputVariantDefault  -> Nothing
    InputVariantUnstyled -> Just "unstyled"
    InputVariantFilled   -> Just "filled"

data InputWrapperOrder
  = InputWrapperOrderInput
  | InputWrapperOrderLabel
  | InputWrapperOrderError
  | InputWrapperOrderDescription

type InputWrapperOrderImpl = String

instance ToFFI InputWrapperOrder InputWrapperOrderImpl where
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

type InputTypeImpl = String

instance ToFFI InputType InputTypeImpl where
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

type InputProps = MantineComponent InputPropsRow

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
  , leftSection               :: Optional JSX
  , leftSectionPointerEvents  :: Optional PointerEvents
  , leftSectionWidth          :: Optional Pixels
  , multiline                 :: Boolean
  , pointer                   :: Boolean
  , radius                    :: Optional MantineNumberSize
  , rightSection              :: Optional JSX
  , rightSectionPointerEvents :: Optional PointerEvents
  , rightSectionWidth         :: Optional Pixels
  , withAria                  :: Boolean
  , withErrorStyles           :: Optional Boolean
  | InputBasePropsRow_ rest
  )

type InputGroupPropsRow items = InputGroupPropsRow_ items ()
type InputGroupPropsRow_ items rest =
  ( children     :: Array JSX
  , labelElement :: Optional InputWrapperElement
  | Controlled_ items + InputBasePropsRow_ rest
  )

type InputBasePropsRow = InputBasePropsRow_ ()
type InputBasePropsRow_ rest =
  ( description       :: Optional JSX
  , error             :: Optional JSX
  , id                :: Optional String
  , inputWrapperOrder :: Optional (Array InputWrapperOrder)
  , label             :: Optional JSX
  , placeholder       :: Optional String
  , required          :: Boolean
  , size              :: Optional MantineSize
  , variant           :: InputVariant
  , withAsterisk      :: Boolean
  | rest
  )

type InputPropsImpl = MantineComponentImpl InputPropsRowImpl

type InputPropsRowImpl = InputPropsRowImpl_ ()
type InputPropsRowImpl_ rest =
  ( disabled                  :: Boolean
  , leftSection               :: OptionalImpl JSX
  , leftSectionPointerEvents  :: OptionalImpl PointerEventsImpl
  , leftSectionWidth          :: OptionalImpl PixelsImpl
  , multiline                 :: Boolean
  , pointer                   :: Boolean
  , radius                    :: OptionalImpl MantineNumberSizeImpl
  , rightSection              :: OptionalImpl JSX
  , rightSectionPointerEvents :: OptionalImpl PointerEventsImpl
  , rightSectionWidth         :: OptionalImpl PixelsImpl
  , withAria                  :: Boolean
  , withErrorStyles           :: OptionalImpl Boolean
  | InputBasePropsRowImpl_ rest
  )

type InputGroupPropsRowImpl items = InputGroupPropsRowImpl_ items ()
type InputGroupPropsRowImpl_ items rest =
  ( children     :: Array JSX
  , labelElement :: OptionalImpl InputWrapperElementImpl
  | ControlledImpl_ items + InputBasePropsRowImpl_ rest
  )

type InputBasePropsRowImpl = InputBasePropsRowImpl_ ()
type InputBasePropsRowImpl_ rest =
  ( description       :: OptionalImpl JSX
  , error             :: OptionalImpl JSX
  , id                :: OptionalImpl String
  , inputWrapperOrder :: OptionalImpl (Array InputWrapperOrderImpl)
  , label             :: OptionalImpl JSX
  , placeholder       :: OptionalImpl String
  , required          :: Boolean
  , size              :: OptionalImpl MantineSizeImpl
  , variant           :: InputVariantImpl
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
  MantineComponent (
    WithInputContainer
      ( children          :: Array JSX
      , description       :: Optional JSX
      , error             :: Optional JSX
      , id                :: Optional String
      , inputWrapperOrder :: Optional (Array InputWrapperOrder)
      , label             :: Optional JSX
      , labelElement      :: Optional InputWrapperElement
      , required          :: Boolean
      , size              :: Optional MantineSize
      , withAsterisk      :: Boolean
      )
  )

data InputWrapperElement = InputWrapperDiv | InputWrapperLabel

type InputWrapperElementImpl = String

instance ToFFI InputWrapperElement InputWrapperElementImpl where
  toNative = case _ of
    InputWrapperDiv   -> "div"
    InputWrapperLabel -> "label"

type InputWrapperPropsImpl =
  MantineComponentImpl (
    WithInputContainerImpl
      ( children          :: Array JSX
      , description       :: OptionalImpl JSX
      , error             :: OptionalImpl JSX
      , id                :: OptionalImpl String
      , inputWrapperOrder :: OptionalImpl (Array InputWrapperOrderImpl)
      , label             :: OptionalImpl JSX
      , labelElement      :: OptionalImpl InputWrapperElementImpl
      , required          :: Boolean
      , size              :: OptionalImpl MantineSizeImpl
      , withAsterisk      :: Boolean
      )
  )

inputLabel :: (InputLabelProps -> InputLabelProps) -> JSX
inputLabel = mkTrivialComponent inputLabelComponent

foreign import inputLabelComponent :: ReactComponent InputLabelPropsImpl

type InputLabelProps =
  MantineComponent
    ( children     :: Array JSX
    , labelElement :: Optional InputWrapperElement
    , required     :: Boolean
    , size         :: Optional MantineSize
    )

type InputLabelPropsImpl =
  MantineComponentImpl
    ( children     :: Array JSX
    , labelElement :: OptionalImpl InputWrapperElementImpl
    , required     :: Boolean
    , size         :: OptionalImpl MantineSizeImpl
    )

inputDescription :: (InputDescriptionProps -> InputDescriptionProps) -> JSX
inputDescription = mkTrivialComponent inputDescriptionComponent

foreign import inputDescriptionComponent :: ReactComponent InputDescriptionPropsImpl

type InputDescriptionProps =
  MantineComponent
    ( children :: Array JSX
    , size     :: Optional MantineSize
    )

type InputDescriptionPropsImpl =
  MantineComponentImpl
    ( children :: Array JSX
    , size     :: OptionalImpl MantineSizeImpl
    )

inputError :: (InputErrorProps -> InputErrorProps) -> JSX
inputError = mkTrivialComponent inputErrorComponent

foreign import inputErrorComponent :: ReactComponent InputErrorPropsImpl

type InputErrorProps =
  MantineComponent
    ( children :: Array JSX
    , size     :: Optional MantineSize
    )

type InputErrorPropsImpl =
  MantineComponentImpl
    ( children :: Array JSX
    , size     :: OptionalImpl MantineSizeImpl
    )

type WithInputContainer rest =
  ( inputContainer :: Optional (JSX -> JSX)
  | rest
  )

type WithInputContainerImpl rest =
  ( inputContainer :: OptionalImpl (JSX -> JSX)
  | rest
  )

type InputComponent rest =
  MantineComponent (WithInputContainer + InputPropsRow_ rest)

type InputComponentImpl rest =
  MantineComponentImpl (WithInputContainerImpl + InputPropsRowImpl_ rest)

type InputGroupComponent items rest =
  MantineComponent (WithInputContainer + InputGroupPropsRow_ items rest)

type InputGroupComponentImpl items rest =
  MantineComponentImpl (WithInputContainerImpl + InputGroupPropsRowImpl_ items rest)
