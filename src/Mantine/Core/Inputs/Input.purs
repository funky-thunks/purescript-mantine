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
  , Props_input

  , inputWrapper
  , Props_InputWrapper
  , Props_InputWrapperImpl
  , InputWrapperElement(..)
  , InputWrapperElementImpl

  , inputLabel
  , Props_InputLabel
  , Props_InputLabelImpl
  , Props_InputLabelInner
  , Props_InputLabelInnerImpl

  , inputDescription
  , Props_InputDescription
  , Props_InputDescriptionImpl
  , Props_InputDescriptionInner
  , Props_InputDescriptionInnerImpl

  , inputError
  , Props_InputError
  , Props_InputErrorImpl
  , Props_InputErrorInner
  , Props_InputErrorInnerImpl

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
--   { leftSectionProps  :: Record<string, any>
--   , rightSectionProps :: Record<string, any>
--   , wrapperProps      :: Record<string, any>
--   }

type Props_InputRow = Props_InputRow_ ()
type Props_InputRow_ rest =
  ( descriptionProps          :: Record Props_InputDescriptionInner
  , errorProps                :: Record Props_InputErrorInner
  , labelProps                :: Record Props_InputLabelInner
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
  ( "aria-label"      :: String
  , description       :: JSX
  , error             :: JSX
  , inputWrapperOrder :: Array InputWrapperOrder
  , label             :: JSX
  , required          :: Boolean
  , size              :: MantineSize
  , variant           :: InputVariant
  , withAsterisk      :: Boolean
  | Props_input rest
  )

type Props_InputImpl = Props_CommonImpl Props_InputRowImpl

type Props_InputRowImpl = Props_InputRowImpl_ ()
type Props_InputRowImpl_ rest =
  ( descriptionProps          :: Record Props_InputDescriptionInnerImpl
  , errorProps                :: Record Props_InputErrorInnerImpl
  , labelProps                :: Record Props_InputLabelInnerImpl
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
  ( "aria-label"      :: String
  , description       :: JSX
  , error             :: JSX
  , inputWrapperOrder :: Array InputWrapperOrderImpl
  , label             :: JSX
  , required          :: Boolean
  , size              :: MantineSizeImpl
  , variant           :: InputVariantImpl
  , withAsterisk      :: Boolean
  | Props_input rest
  )

-- This base rowlist is a ripoff of react-basic-dom
type Props_input rest =
  ( autoCapitalize        :: String
  , autoComplete          :: String
  , autoCorrect           :: String
  , autoFocus             :: Boolean
  , autoPlay              :: Boolean
  , autoSave              :: String
  , disabled              :: Boolean
  , form                  :: String
  , formAction            :: String
  , formEncType           :: String
  , formMethod            :: String
  , formNoValidate        :: Boolean
  , formTarget            :: String
  , id                    :: String
  , inputMode             :: String
  , name                  :: String
  , noValidate            :: Boolean
  , onAnimationEnd        :: EventHandler
  , onAnimationIteration  :: EventHandler
  , onAnimationStart      :: EventHandler
  , onBlur                :: EventHandler
  , onClick               :: EventHandler
  , onCompositionEnd      :: EventHandler
  , onCompositionStart    :: EventHandler
  , onCompositionUpdate   :: EventHandler
  , onContextMenu         :: EventHandler
  , onCopy                :: EventHandler
  , onCut                 :: EventHandler
  , onDoubleClick         :: EventHandler
  , onDrag                :: EventHandler
  , onDragEnd             :: EventHandler
  , onDragEnter           :: EventHandler
  , onDragExit            :: EventHandler
  , onDragLeave           :: EventHandler
  , onDragOver            :: EventHandler
  , onDragStart           :: EventHandler
  , onDrop                :: EventHandler
  , onFocus               :: EventHandler
  , onGotPointerCapture   :: EventHandler
  , onInvalid             :: EventHandler
  , onKeyDown             :: EventHandler
  , onKeyPress            :: EventHandler
  , onKeyUp               :: EventHandler
  , onLostPointerCapture  :: EventHandler
  , onMouseDown           :: EventHandler
  , onMouseEnter          :: EventHandler
  , onMouseLeave          :: EventHandler
  , onMouseMove           :: EventHandler
  , onMouseOut            :: EventHandler
  , onMouseOver           :: EventHandler
  , onMouseUp             :: EventHandler
  , onPaste               :: EventHandler
  , onPointerCancel       :: EventHandler
  , onPointerDown         :: EventHandler
  , onPointerEnter        :: EventHandler
  , onPointerLeave        :: EventHandler
  , onPointerMove         :: EventHandler
  , onPointerOut          :: EventHandler
  , onPointerOver         :: EventHandler
  , onPointerUp           :: EventHandler
  , onSelect              :: EventHandler
  , onSubmit              :: EventHandler
  , onTouchCancel         :: EventHandler
  , onTouchEnd            :: EventHandler
  , onTouchMove           :: EventHandler
  , onTouchStart          :: EventHandler
  , onTransitionEnd       :: EventHandler
  , onWheel               :: EventHandler
  , pattern               :: String
  , placeholder           :: String
  , readOnly              :: Boolean
  , title                 :: String
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

type Props_InputWrapper =
  Props_Common (
    WithInputContainer
      ( children          :: Array JSX
      , description       :: JSX
      , descriptionProps  :: Record Props_InputDescriptionInner
      , error             :: JSX
      , errorProps        :: Record Props_InputErrorInner
      , id                :: String
      , inputWrapperOrder :: Array InputWrapperOrder
      , label             :: JSX
      , labelElement      :: InputWrapperElement
      , labelProps        :: Record Props_InputLabelInner
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
      , descriptionProps  :: Record Props_InputDescriptionInnerImpl
      , error             :: JSX
      , errorProps        :: Record Props_InputErrorInnerImpl
      , id                :: String
      , inputWrapperOrder :: Array InputWrapperOrderImpl
      , label             :: JSX
      , labelElement      :: InputWrapperElementImpl
      , labelProps        :: Record Props_InputLabelInnerImpl
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

type Props_InputLabelInner =
  ( labelElement :: Optional InputWrapperElement
  , required     :: Optional Boolean
  , size         :: Optional MantineSize
  )

type Props_InputLabelInnerImpl =
  ( labelElement :: OptionalImpl InputWrapperElementImpl
  , required     :: OptionalImpl Boolean
  , size         :: OptionalImpl MantineSizeImpl
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

type Props_InputDescriptionInner =
  ( size :: Optional MantineSize
  )

type Props_InputDescriptionInnerImpl =
  ( size :: OptionalImpl MantineSizeImpl
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

type Props_InputErrorInner =
  ( size :: Optional MantineSize
  )

type Props_InputErrorInnerImpl =
  ( size :: OptionalImpl MantineSizeImpl
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
