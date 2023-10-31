module Mantine.Core.Inputs.NumberInput
  ( numberInput
  , NumberClampBehavior(..)
  , NumberInput(..)
  , NumberInputProps
  , NumberInputHandlers
  , NumberInputType(..)
  , ThousandsGroupStyle(..)
  ) where

import Data.Either (Either(..))
import Mantine.Core.Inputs.Input (InputComponent, InputComponentImpl)
import Mantine.Core.Prelude
import Untagged.Union (toEither1)

numberInput :: (NumberInputProps -> NumberInputProps) -> JSX
numberInput = mkTrivialComponent numberInputComponent

foreign import numberInputComponent :: ReactComponent NumberInputPropsImpl

-- Not supported properties:
-- { descriptionProps :: Record<string, any>
-- , isAllowed        :: NumberFormatValues -> Boolean
-- , onValueChange    :: OnValueChange -- Called when value changes with react-number-format payload
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
    , defaultValue             :: Maybe NumberInput
    , fixedDecimalScale        :: Boolean
    , handlersRef              :: Maybe (Ref NumberInputHandlers)
    , hideControls             :: Boolean
    , max                      :: Maybe Number
    , min                      :: Maybe Number
    , onChange                 :: ValueHandler NumberInput
    , prefix                   :: Maybe String
    , startValue               :: Maybe Number
    , step                     :: Maybe Number
    , suffix                   :: Maybe String
    , thousandSeparator        :: Maybe String
    , thousandsGroupStyle      :: Maybe ThousandsGroupStyle
    , type                     :: NumberInputType
    , value                    :: Maybe NumberInput
    , valueIsNumericString     :: Boolean
    )

data NumberClampBehavior
  = NumberClampBehaviorNone
  | NumberClampBehaviorBlur
  | NumberClampBehaviorStrict

instance ToFFI NumberClampBehavior String where
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

instance ToFFI ThousandsGroupStyle String where
  toNative = case _ of
    ThousandsGroupStyleNone     -> "none"
    ThousandsGroupStyleThousand -> "thousand"
    ThousandsGroupStyleLakh     -> "lakh"
    ThousandsGroupStyleWan      -> "wan"

data NumberInputType = NumberInputTypeText | NumberInputTypeNumber

instance DefaultValue NumberInputType where defaultValue = NumberInputTypeText

instance ToFFI NumberInputType String where
  toNative = case _ of
    NumberInputTypeText   -> "text"
    NumberInputTypeNumber -> "number"

type NumberInputPropsImpl =
  InputComponentImpl
    ( allowDecimal             :: Boolean
    , allowLeadingZeros        :: Boolean
    , allowNegative            :: Boolean
    , allowedDecimalSeparators :: Nullable (Array String)
    , clampBehavior            :: Nullable String
    , decimalScale             :: Nullable Number
    , decimalSeparator         :: Nullable String
    , defaultValue             :: Nullable (Number |+| String)
    , fixedDecimalScale        :: Boolean
    , handlersRef              :: Nullable (Ref NumberInputHandlers)
    , hideControls             :: Boolean
    , max                      :: Nullable Number
    , min                      :: Nullable Number
    , onChange                 :: EffectFn1 (Number |+| String) Unit
    , prefix                   :: Nullable String
    , startValue               :: Nullable Number
    , step                     :: Nullable Number
    , suffix                   :: Nullable String
    , thousandSeparator        :: Nullable String
    , thousandsGroupStyle      :: Nullable String
    , type                     :: String
    , value                    :: Nullable (Number |+| String)
    , valueIsNumericString     :: Boolean
    )
