module Mantine.Core.Inputs.NumberInput
  ( numberInput
  , NumberInputProps
  , NumberInputHandlers
  , NumberFormat
  , NumberFormatter
  , NumberParser
  , NumberInputType(..)
  , StepHoldInterval(..)

  , module Mantine.Core.Inputs.Input
  ) where

import Mantine.Core.Prelude
import Mantine.Core.Inputs.Input (InputVariant(..), InputWrapperOrder(..))
import Untagged.Union (maybeToUor, uorToMaybe)

numberInput :: (NumberInputProps -> NumberInputProps) -> JSX
numberInput = mkComponent numberInputComponent toNumberImpl defaultThemingProps_

foreign import numberInputComponent :: ReactComponent NumberInputPropsImpl

-- Not supported properties:
-- { descriptionProps  :: Record<string, any>
-- , errorProps        :: Record<string, any>
-- , inputContainer    :: (children: ReactNode) => ReactNode
-- , labelProps        :: Record<string, any>
-- , rightSectionProps :: Record<string, any>
-- , wrapperProps      :: Record<string, any>
-- }

type NumberInputProps =
  ThemingProps
    ( decimalSeparator    :: Maybe String
    , defaultValue        :: Maybe Number
    , description         :: Maybe JSX
    , disabled            :: Boolean
    , error               :: Maybe JSX
    , format              :: Maybe NumberFormat
    , handlersRef         :: Maybe (Ref NumberInputHandlers)
    , hideControls        :: Boolean
    , icon                :: Maybe JSX
    , iconWidth           :: Maybe Pixels
    , inputWrapperOrder   :: Maybe (Array InputWrapperOrder)
    , label               :: Maybe JSX
    , max                 :: Maybe Number
    , min                 :: Maybe Number
    , noClampOnBlur       :: Boolean
    , onChange            :: ValueHandler Number
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
    , type                :: NumberInputType
    , value               :: Maybe Number
    , variant             :: InputVariant
    , withAsterisk        :: Boolean
    )

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
    , defaultValue        :: Nullable Number
    , description         :: Nullable JSX
    , disabled            :: Boolean
    , error               :: Nullable JSX
    , formatter           :: Nullable NumberFormatterImpl
    , handlersRef         :: Nullable (Ref NumberInputHandlers)
    , hideControls        :: Boolean
    , icon                :: Nullable JSX
    , iconWidth           :: Nullable Number
    , inputWrapperOrder   :: Nullable (Array String)
    , label               :: Nullable JSX
    , max                 :: Nullable Number
    , min                 :: Nullable Number
    , noClampOnBlur       :: Boolean
    , onChange            :: EffectFn1 Number Unit
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
    , type                :: String
    , value               :: Nullable Number
    , variant             :: String
    , withAsterisk        :: Boolean
    )

type NumberFormatterImpl = UndefinedOr String -> String

formatterToNative :: NumberFormatter -> NumberFormatterImpl
formatterToNative f = toNative <<< f <<< undefinedableToMaybe

type NumberParserImpl = UndefinedOr String -> UndefinedOr String

parserToNative :: NumberParser -> NumberParserImpl
parserToNative f = maybeToUndefinedable <<< f <<< undefinedableToMaybe

maybeToUndefinedable :: forall js purs. ToFFI purs js => Maybe purs -> UndefinedOr js
maybeToUndefinedable = maybeToUor <<< map toNative

undefinedableToMaybe :: forall js purs. FromFFI js purs => UndefinedOr js -> Maybe purs
undefinedableToMaybe = map fromNative <<< uorToMaybe

toNumberImpl :: NumberInputProps -> NumberInputPropsImpl
toNumberImpl props =
  let rest = toNative <<< delete (Proxy :: Proxy "format")
      formatter = toNullable $ formatterToNative <<< _.formatter <$> props.format
      parser    = toNullable $ parserToNative    <<< _.parser    <$> props.format
   in { formatter, parser } `union` rest props
