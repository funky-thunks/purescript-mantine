module Mantine.Core.DataDisplay.NumberFormatter
  ( numberFormatter
  , NumberFormatterProps
  , NumberFormatterValue(..)
  ) where

import Mantine.Core.Inputs.NumberInput (ThousandsGroupStyle, ThousandsGroupStyleImpl, ThousandSeparator, ThousandSeparatorImpl)
import Mantine.Core.Prelude

numberFormatter :: (NumberFormatterProps -> NumberFormatterProps) -> JSX
numberFormatter = mkTrivialComponent numberFormatterComponent

foreign import numberFormatterComponent :: ReactComponent NumberFormatterPropsImpl

type NumberFormatterProps =
  MantineComponent
    ( allowNegative       :: Boolean
    , decimalScale        :: Optional Number
    , decimalSeparator    :: Optional String
    , fixedDecimalScale   :: Boolean
    , prefix              :: Optional String
    , suffix              :: Optional String
    , thousandSeparator   :: Optional ThousandSeparator
    , thousandsGroupStyle :: Optional ThousandsGroupStyle
    , value               :: Optional NumberFormatterValue
    )

data NumberFormatterValue
  = AsNumber Number
  | AsString String

type NumberFormatterValueImpl = String |+| Number

instance ToFFI NumberFormatterValue NumberFormatterValueImpl where
  toNative = case _ of
    AsNumber n -> asOneOf n
    AsString s -> asOneOf s

type NumberFormatterPropsImpl =
  MantineComponentImpl
    ( allowNegative       :: Boolean
    , decimalScale        :: OptionalImpl Number
    , decimalSeparator    :: OptionalImpl String
    , fixedDecimalScale   :: Boolean
    , prefix              :: OptionalImpl String
    , suffix              :: OptionalImpl String
    , thousandSeparator   :: OptionalImpl ThousandSeparatorImpl
    , thousandsGroupStyle :: OptionalImpl ThousandsGroupStyleImpl
    , value               :: OptionalImpl NumberFormatterValueImpl
    )
