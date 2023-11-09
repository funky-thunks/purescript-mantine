module Mantine.Core.Inputs.NumberInput
  ( numberInput
  , NumberClampBehavior(..)
  , NumberInput(..)
  , NumberInputProps
  , NumberInputHandlers
  , NumberInputType(..)
  , ThousandsGroupStyle(..)
  , ThousandSeparator(..)

  , ThousandSeparatorImpl
  , ThousandsGroupStyleImpl
  ) where

import Data.Either (Either(..))
import Mantine.Core.Inputs.Input (InputComponent, InputComponentImpl)
import Mantine.Core.Prelude
import Untagged.Union (toEither1)

numberInput :: (NumberInputProps -> NumberInputProps) -> JSX
numberInput = mkTrivialComponent numberInputComponent

foreign import numberInputComponent :: ReactComponent NumberInputPropsImpl

-- Not supported properties:
-- { isAllowed     :: NumberFormatValues -> Boolean
-- , onValueChange :: OnValueChange -- Called when value changes with react-number-format payload
-- }

type NumberInputProps =
  InputComponent
    ( allowDecimal             :: Boolean
    , allowLeadingZeros        :: Boolean
    , allowNegative            :: Boolean
    , allowedDecimalSeparators :: Maybe (Array String)
    , clampBehavior            :: Maybe NumberClampBehavior
    , decimalScale             :: Maybe Number
    , decimalSeparator         :: Maybe String
    , fixedDecimalScale        :: Boolean
    , handlersRef              :: Maybe (Ref NumberInputHandlers)
    , hideControls             :: Boolean
    , max                      :: Maybe Number
    , min                      :: Maybe Number
    , prefix                   :: Maybe String
    , startValue               :: Maybe Number
    , step                     :: Maybe Number
    , suffix                   :: Maybe String
    , thousandSeparator        :: Maybe ThousandSeparator
    , thousandsGroupStyle      :: Maybe ThousandsGroupStyle
    , type                     :: NumberInputType
    , valueIsNumericString     :: Boolean
    | Controlled NumberInput
    )

data NumberClampBehavior
  = NumberClampBehaviorNone
  | NumberClampBehaviorBlur
  | NumberClampBehaviorStrict

type NumberClampBehaviorImpl = String

instance ToFFI NumberClampBehavior NumberClampBehaviorImpl where
  toNative = case _ of
    NumberClampBehaviorNone   -> "none"
    NumberClampBehaviorBlur   -> "blur"
    NumberClampBehaviorStrict -> "strict"

data NumberInput = ValidInput Number | Invalid

type NumberInputImpl = Number |+| String

instance ToFFI NumberInput NumberInputImpl where
  toNative = case _ of
    ValidInput n -> asOneOf n
    Invalid      -> asOneOf ""

instance FromFFI NumberInputImpl NumberInput where
  fromNative = toEither1 >>> case _ of
    Left  n -> ValidInput n
    Right _ -> Invalid

type NumberInputHandlers =
  { increment :: Effect Unit
  , decrement :: Effect Unit
  }

data ThousandsGroupStyle
  = ThousandsGroupStyleNone
  | ThousandsGroupStyleThousand
  | ThousandsGroupStyleLakh
  | ThousandsGroupStyleWan

type ThousandsGroupStyleImpl = String

instance ToFFI ThousandsGroupStyle ThousandsGroupStyleImpl where
  toNative = case _ of
    ThousandsGroupStyleNone     -> "none"
    ThousandsGroupStyleThousand -> "thousand"
    ThousandsGroupStyleLakh     -> "lakh"
    ThousandsGroupStyleWan      -> "wan"

data ThousandSeparator
  = DefaultThousandSeparator
  | CustomThousandSeparator String

type ThousandSeparatorImpl = String |+| Boolean

instance ToFFI ThousandSeparator ThousandSeparatorImpl where
  toNative = case _ of
    DefaultThousandSeparator  -> asOneOf true
    CustomThousandSeparator s -> asOneOf s

data NumberInputType = NumberInputTypeText | NumberInputTypeNumber

instance DefaultValue NumberInputType where defaultValue = NumberInputTypeText

type NumberInputTypeImpl = String

instance ToFFI NumberInputType NumberInputTypeImpl where
  toNative = case _ of
    NumberInputTypeText   -> "text"
    NumberInputTypeNumber -> "number"

type NumberInputPropsImpl =
  InputComponentImpl
    ( allowDecimal             :: Boolean
    , allowLeadingZeros        :: Boolean
    , allowNegative            :: Boolean
    , allowedDecimalSeparators :: Nullable (Array String)
    , clampBehavior            :: Nullable NumberClampBehaviorImpl
    , decimalScale             :: Nullable Number
    , decimalSeparator         :: Nullable String
    , fixedDecimalScale        :: Boolean
    , handlersRef              :: Nullable (Ref NumberInputHandlers)
    , hideControls             :: Boolean
    , max                      :: Nullable Number
    , min                      :: Nullable Number
    , prefix                   :: Nullable String
    , startValue               :: Nullable Number
    , step                     :: Nullable Number
    , suffix                   :: Nullable String
    , thousandSeparator        :: Nullable ThousandSeparatorImpl
    , thousandsGroupStyle      :: Nullable ThousandsGroupStyleImpl
    , type                     :: NumberInputTypeImpl
    , valueIsNumericString     :: Boolean
    | ControlledImpl NumberInputImpl
    )
