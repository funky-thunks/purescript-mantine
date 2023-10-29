module Mantine.Core.Inputs.NumberInput
  ( numberInput
  , NumberInput(..)
  , NumberInputProps
  , NumberInputHandlers
  , NumberFormat
  , NumberFormatter
  , NumberParser
  , NumberInputType(..)
  , StepHoldInterval(..)

  , module Mantine.Core.Inputs.Input
  ) where

import Data.Either (Either(..))
import Data.Maybe (fromMaybe)
import Mantine.Core.Inputs.Input (InputVariant(..), InputWrapperOrder(..))
import Mantine.Core.Prelude
import Untagged.Union (toEither1)

numberInput :: (NumberInputProps -> NumberInputProps) -> JSX
numberInput = mkComponent numberInputComponent toNumberImpl defaultThemingProps_

foreign import numberInputComponent :: ReactComponent NumberInputPropsImpl

-- Not supported properties:
-- { descriptionProps  :: Record<string, any>
-- , errorProps        :: Record<string, any>
-- , labelProps        :: Record<string, any>
-- , rightSectionProps :: Record<string, any>
-- , wrapperProps      :: Record<string, any>
-- }

type NumberInputProps =
  ThemingProps
    ( decimalSeparator    :: Maybe String
    , defaultValue        :: Maybe NumberInput
    , description         :: Maybe JSX
    , disabled            :: Boolean
    , error               :: Maybe JSX
    , format              :: Maybe NumberFormat
    , handlersRef         :: Maybe (Ref NumberInputHandlers)
    , hideControls        :: Boolean
    , icon                :: Maybe JSX
    , iconWidth           :: Maybe Pixels
    , inputContainer      :: Maybe (JSX -> JSX)
    , inputWrapperOrder   :: Maybe (Array InputWrapperOrder)
    , label               :: Maybe JSX
    , max                 :: Maybe Number
    , min                 :: Maybe Number
    , noClampOnBlur       :: Boolean
    , onChange            :: ValueHandler NumberInput
    , placeholder         :: Maybe String
    , precision           :: Maybe Number
    , radius              :: Maybe MantineNumberSize
    , removeTrailingZeros :: Boolean
    , required            :: Boolean
    , rightSection        :: Maybe JSX
    , rightSectionWidth   :: Maybe Pixels
    , size                :: Maybe MantineSize
    , startValue          :: Maybe Number
    , step                :: Maybe Number
    , stepHoldDelay       :: Maybe Milliseconds
    , stepHoldInterval    :: Maybe StepHoldInterval
    , thousandsSeparator  :: Maybe String
    , type                :: NumberInputType
    , value               :: Maybe NumberInput
    , variant             :: InputVariant
    , withAsterisk        :: Boolean
    )

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

type NumberFormat =
  { formatter :: NumberFormatter
  , parser    :: NumberParser
  }

type NumberFormatter = Maybe String -> String

type NumberParser = Maybe String -> Maybe String

data StepHoldInterval = StepHoldIntervalStatic Milliseconds
                      | StepHoldIntervalDynamic (Number -> Milliseconds)

type StepHoldIntervalImpl = Milliseconds |+| (Number -> Milliseconds)

instance ToFFI StepHoldInterval StepHoldIntervalImpl where
  toNative = case _ of
    StepHoldIntervalStatic  s -> asOneOf s
    StepHoldIntervalDynamic f -> asOneOf f

data NumberInputType = NumberInputTypeText | NumberInputTypeNumber

instance DefaultValue NumberInputType where defaultValue = NumberInputTypeText

instance ToFFI NumberInputType String where
  toNative = case _ of
    NumberInputTypeText   -> "text"
    NumberInputTypeNumber -> "number"

type NumberInputPropsImpl =
  ThemingPropsImpl
    ( decimalSeparator    :: Nullable String
    , defaultValue        :: Nullable (Number |+| String)
    , description         :: Nullable JSX
    , disabled            :: Boolean
    , error               :: Nullable JSX
    , formatter           :: Nullable NumberFormatterImpl
    , handlersRef         :: Nullable (Ref NumberInputHandlers)
    , hideControls        :: Boolean
    , icon                :: Nullable JSX
    , iconWidth           :: Nullable Number
    , inputContainer      :: Nullable (JSX -> JSX)
    , inputWrapperOrder   :: Nullable (Array String)
    , label               :: Nullable JSX
    , max                 :: Nullable Number
    , min                 :: Nullable Number
    , noClampOnBlur       :: Boolean
    , onChange            :: EffectFn1 (Number |+| String) Unit
    , parser              :: Nullable NumberParserImpl
    , placeholder         :: Nullable String
    , precision           :: Nullable Number
    , radius              :: Nullable MantineNumberSizeImpl
    , removeTrailingZeros :: Boolean
    , required            :: Boolean
    , rightSection        :: Nullable JSX
    , rightSectionWidth   :: Nullable Number
    , size                :: Nullable String
    , startValue          :: Nullable Number
    , step                :: Nullable Number
    , stepHoldDelay       :: Nullable Number
    , stepHoldInterval    :: Nullable StepHoldIntervalImpl
    , thousandsSeparator  :: Nullable String
    , type                :: String
    , value               :: Nullable (Number |+| String)
    , variant             :: String
    , withAsterisk        :: Boolean
    )

type NumberFormatterImpl = String -> String

formatterToNative :: NumberFormatter -> NumberFormatterImpl
formatterToNative f = f <<< nonEmptyString

type NumberParserImpl = String -> String

parserToNative :: NumberParser -> NumberParserImpl
parserToNative f = fromMaybe "" <<< f <<< nonEmptyString

nonEmptyString :: String -> Maybe String
nonEmptyString = case _ of
  "" -> Nothing
  s  -> Just s

toNumberImpl :: NumberInputProps -> NumberInputPropsImpl
toNumberImpl props =
  let rest = toNative <<< delete (Proxy :: Proxy "format")
                      <<< delete (Proxy :: Proxy "inputContainer")
      formatter = toNullable $ formatterToNative <<< _.formatter <$> props.format
      parser    = toNullable $ parserToNative    <<< _.parser    <$> props.format
      inputContainer = toNullable props.inputContainer
   in { formatter, parser, inputContainer } `union` rest props
