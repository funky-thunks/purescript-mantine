module Mantine.Core.Inputs.NumberInput
  ( numberInput
  , Props_NumberInput
  , Props_NumberInputImpl
  , NumberClampBehavior(..)
  , NumberClampBehaviorImpl
  , NumberInput(..)
  , NumberInputImpl
  , NumberInputHandlers
  , NumberInputType(..)
  , NumberInputTypeImpl
  , ThousandsGroupStyle(..)
  , ThousandsGroupStyleImpl
  , ThousandSeparator(..)
  , ThousandSeparatorImpl
  ) where

import Data.Either (Either(..))
import Mantine.Core.Inputs.Input (Props_InputComponent, Props_InputComponentImpl)
import Mantine.Core.Prelude
import Untagged.Union (toEither1)

numberInput
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_NumberInput
  => Union attrsImpl attrsImpl_ Props_NumberInputImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
numberInput = element (unsafeCoerce numberInputComponent) <<< toNative

foreign import numberInputComponent :: ReactComponent (Record Props_NumberInputImpl)

-- Not supported properties:
-- { isAllowed     :: NumberFormatValues -> Boolean
-- , onValueChange :: OnValueChange -- Called when value changes with react-number-format payload
-- }

type Props_NumberInput =
  Props_InputComponent
    ( allowDecimal             :: Boolean
    , allowLeadingZeros        :: Boolean
    , allowNegative            :: Boolean
    , allowedDecimalSeparators :: Array String
    , clampBehavior            :: NumberClampBehavior
    , decimalScale             :: Number
    , decimalSeparator         :: String
    , fixedDecimalScale        :: Boolean
    , handlersRef              :: Ref NumberInputHandlers
    , hideControls             :: Boolean
    , max                      :: Number
    , min                      :: Number
    , prefix                   :: String
    , startValue               :: Number
    , step                     :: Number
    , suffix                   :: String
    , thousandSeparator        :: ThousandSeparator
    , thousandsGroupStyle      :: ThousandsGroupStyle
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

data NumberInput = ValidInput Number | InvalidInput

type NumberInputImpl = Number |+| String

instance ToFFI NumberInput NumberInputImpl where
  toNative = case _ of
    ValidInput n -> asOneOf n
    InvalidInput -> asOneOf ""

instance FromFFI NumberInputImpl NumberInput where
  fromNative = toEither1 >>> case _ of
    Left  n -> ValidInput n
    Right _ -> InvalidInput

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

type NumberInputTypeImpl = String

instance ToFFI NumberInputType NumberInputTypeImpl where
  toNative = case _ of
    NumberInputTypeText   -> "text"
    NumberInputTypeNumber -> "number"

type Props_NumberInputImpl =
  Props_InputComponentImpl
    ( allowDecimal             :: Boolean
    , allowLeadingZeros        :: Boolean
    , allowNegative            :: Boolean
    , allowedDecimalSeparators :: Array String
    , clampBehavior            :: NumberClampBehaviorImpl
    , decimalScale             :: Number
    , decimalSeparator         :: String
    , fixedDecimalScale        :: Boolean
    , handlersRef              :: Ref NumberInputHandlers
    , hideControls             :: Boolean
    , max                      :: Number
    , min                      :: Number
    , prefix                   :: String
    , startValue               :: Number
    , step                     :: Number
    , suffix                   :: String
    , thousandSeparator        :: ThousandSeparatorImpl
    , thousandsGroupStyle      :: ThousandsGroupStyleImpl
    , type                     :: NumberInputTypeImpl
    , valueIsNumericString     :: Boolean
    | ControlledImpl NumberInputImpl
    )
