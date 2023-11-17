module Mantine.Core.Inputs.Input
  ( InputVariant(..)
  , InputVariantImpl
  , InputWrapperOrder(..)
  , InputWrapperOrderImpl
  , InputType(..)
  , InputTypeImpl

  , input
  , Props_Input
  , Props_InputImpl
  , Props_InputRow
  , Props_InputRowImpl
  , Props_InputRow_
  , Props_InputRowImpl_
  , Props_InputGroupRow
  , Props_InputGroupRowImpl
  , Props_InputGroupRow_
  , Props_InputGroupRowImpl_
  , Props_InputBaseRow
  , Props_InputBaseRowImpl
  , Props_InputBaseRow_
  , Props_InputBaseRowImpl_

  , inputWrapper
  , Props_InputWrapper
  , Props_InputWrapperImpl
  , InputWrapperElement(..)
  , InputWrapperElementImpl

  , inputLabel
  , Props_InputLabel
  , Props_InputLabelImpl

  , inputDescription
  , Props_InputDescription
  , Props_InputDescriptionImpl

  , inputError
  , Props_InputError
  , Props_InputErrorImpl

  , Props_InputComponent
  , Props_InputComponentImpl
  , Props_InputGroupComponent
  , Props_InputGroupComponentImpl
  , WithInputContainer
  , WithInputContainerImpl
  ) where

import Mantine.Core.Prelude

data InputVariant
  = InputVariantDefault
  | InputVariantUnstyled
  | InputVariantFilled

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

input
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Input
  => Union attrsImpl attrsImpl_ Props_InputImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
input = element (unsafeCoerce inputComponent) <<< toNative

foreign import inputComponent :: ReactComponent (Record Props_InputImpl)

type Props_Input = Props_Common Props_InputRow

-- Not supported properties
--   { descriptionProps  :: Record<string, any>
--   , errorProps        :: Record<string, any>
--   , labelProps        :: Record<string, any>
--   , leftSectionProps  :: Record<string, any>
--   , rightSectionProps :: Record<string, any>
--   , wrapperProps      :: Record<string, any>
--   }

type Props_InputRow = Props_InputRow_ ()
type Props_InputRow_ rest =
  ( disabled                  :: Boolean
  , leftSection               :: JSX
  , leftSectionPointerEvents  :: PointerEvents
  , leftSectionWidth          :: Pixels
  , multiline                 :: Boolean
  , pointer                   :: Boolean
  , radius                    :: MantineNumberSize
  , rightSection              :: JSX
  , rightSectionPointerEvents :: PointerEvents
  , rightSectionWidth         :: Pixels
  , withAria                  :: Boolean
  , withErrorStyles           :: Boolean
  | Props_InputBaseRow_ rest
  )

type Props_InputGroupRow items = Props_InputGroupRow_ items ()
type Props_InputGroupRow_ items rest =
  ( children     :: Array JSX
  , labelElement :: InputWrapperElement
  | Controlled_ items + Props_InputBaseRow_ rest
  )

type Props_InputBaseRow = Props_InputBaseRow_ ()
type Props_InputBaseRow_ rest =
  ( description       :: JSX
  , error             :: JSX
  , id                :: String
  , inputWrapperOrder :: Array InputWrapperOrder
  , label             :: JSX
  , placeholder       :: String
  , required          :: Boolean
  , size              :: MantineSize
  , variant           :: InputVariant
  , withAsterisk      :: Boolean
  | rest
  )

type Props_InputImpl = Props_CommonImpl Props_InputRowImpl

type Props_InputRowImpl = Props_InputRowImpl_ ()
type Props_InputRowImpl_ rest =
  ( disabled                  :: Boolean
  , leftSection               :: JSX
  , leftSectionPointerEvents  :: PointerEventsImpl
  , leftSectionWidth          :: PixelsImpl
  , multiline                 :: Boolean
  , pointer                   :: Boolean
  , radius                    :: MantineNumberSizeImpl
  , rightSection              :: JSX
  , rightSectionPointerEvents :: PointerEventsImpl
  , rightSectionWidth         :: PixelsImpl
  , withAria                  :: Boolean
  , withErrorStyles           :: Boolean
  | Props_InputBaseRowImpl_ rest
  )

type Props_InputGroupRowImpl items = Props_InputGroupRowImpl_ items ()
type Props_InputGroupRowImpl_ items rest =
  ( children     :: Array JSX
  , labelElement :: InputWrapperElementImpl
  | ControlledImpl_ items + Props_InputBaseRowImpl_ rest
  )

type Props_InputBaseRowImpl = Props_InputBaseRowImpl_ ()
type Props_InputBaseRowImpl_ rest =
  ( description       :: JSX
  , error             :: JSX
  , id                :: String
  , inputWrapperOrder :: Array InputWrapperOrderImpl
  , label             :: JSX
  , placeholder       :: String
  , required          :: Boolean
  , size              :: MantineSizeImpl
  , variant           :: InputVariantImpl
  , withAsterisk      :: Boolean
  | rest
  )

inputWrapper
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_InputWrapper
  => Union attrsImpl attrsImpl_ Props_InputWrapperImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
inputWrapper = element (unsafeCoerce inputWrapperComponent) <<< toNative

foreign import inputWrapperComponent :: ReactComponent (Record Props_InputWrapperImpl)

-- Not supported properties
--   { descriptionProps :: Record<string, any>
--   , errorProps       :: Record<string, any>
--   , labelProps       :: Record<string, any>
--   }

type Props_InputWrapper =
  Props_Common (
    WithInputContainer
      ( children          :: Array JSX
      , description       :: JSX
      , error             :: JSX
      , id                :: String
      , inputWrapperOrder :: Array InputWrapperOrder
      , label             :: JSX
      , labelElement      :: InputWrapperElement
      , required          :: Boolean
      , size              :: MantineSize
      , withAsterisk      :: Boolean
      )
  )

data InputWrapperElement = InputWrapperDiv | InputWrapperLabel

type InputWrapperElementImpl = String

instance ToFFI InputWrapperElement InputWrapperElementImpl where
  toNative = case _ of
    InputWrapperDiv   -> "div"
    InputWrapperLabel -> "label"

type Props_InputWrapperImpl =
  Props_CommonImpl (
    WithInputContainerImpl
      ( children          :: Array JSX
      , description       :: JSX
      , error             :: JSX
      , id                :: String
      , inputWrapperOrder :: Array InputWrapperOrderImpl
      , label             :: JSX
      , labelElement      :: InputWrapperElementImpl
      , required          :: Boolean
      , size              :: MantineSizeImpl
      , withAsterisk      :: Boolean
      )
  )

inputLabel
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_InputLabel
  => Union attrsImpl attrsImpl_ Props_InputLabelImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
inputLabel = element (unsafeCoerce inputLabelComponent) <<< toNative

foreign import inputLabelComponent :: ReactComponent (Record Props_InputLabelImpl)

type Props_InputLabel =
  Props_Common
    ( children     :: Array JSX
    , labelElement :: InputWrapperElement
    , required     :: Boolean
    , size         :: MantineSize
    )

type Props_InputLabelImpl =
  Props_CommonImpl
    ( children     :: Array JSX
    , labelElement :: InputWrapperElementImpl
    , required     :: Boolean
    , size         :: MantineSizeImpl
    )

inputDescription
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_InputDescription
  => Union attrsImpl attrsImpl_ Props_InputDescriptionImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
inputDescription = element (unsafeCoerce inputDescriptionComponent) <<< toNative

foreign import inputDescriptionComponent :: ReactComponent (Record Props_InputDescriptionImpl)

type Props_InputDescription =
  Props_Common
    ( children :: Array JSX
    , size     :: MantineSize
    )

type Props_InputDescriptionImpl =
  Props_CommonImpl
    ( children :: Array JSX
    , size     :: MantineSizeImpl
    )

inputError
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_InputError
  => Union attrsImpl attrsImpl_ Props_InputErrorImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
inputError = element (unsafeCoerce inputErrorComponent) <<< toNative

foreign import inputErrorComponent :: ReactComponent (Record Props_InputErrorImpl)

type Props_InputError =
  Props_Common
    ( children :: Array JSX
    , size     :: MantineSize
    )

type Props_InputErrorImpl =
  Props_CommonImpl
    ( children :: Array JSX
    , size     :: MantineSizeImpl
    )

type WithInputContainer rest =
  ( inputContainer :: JSX -> JSX
  | rest
  )

type WithInputContainerImpl rest =
  ( inputContainer :: JSX -> JSX
  | rest
  )

type Props_InputComponent rest =
  Props_Common + WithInputContainer + Props_InputRow_ rest

type Props_InputComponentImpl rest =
  Props_CommonImpl + WithInputContainerImpl + Props_InputRowImpl_ rest

type Props_InputGroupComponent items rest =
  Props_Common + WithInputContainer + Props_InputGroupRow_ items rest

type Props_InputGroupComponentImpl items rest =
  Props_CommonImpl + WithInputContainerImpl + Props_InputGroupRowImpl_ items rest
