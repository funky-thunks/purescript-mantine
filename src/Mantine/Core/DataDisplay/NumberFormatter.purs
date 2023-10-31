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
    , decimalScale        :: Maybe Number
    , decimalSeparator    :: Maybe String
    , fixedDecimalScale   :: Boolean
    , prefix              :: Maybe String
    , suffix              :: Maybe String
    , thousandSeparator   :: Maybe ThousandSeparator
    , thousandsGroupStyle :: Maybe ThousandsGroupStyle
    , value               :: Maybe NumberFormatterValue
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
    , decimalScale        :: Nullable Number
    , decimalSeparator    :: Nullable String
    , fixedDecimalScale   :: Boolean
    , prefix              :: Nullable String
    , suffix              :: Nullable String
    , thousandSeparator   :: Nullable ThousandSeparatorImpl
    , thousandsGroupStyle :: Nullable ThousandsGroupStyleImpl
    , value               :: Nullable NumberFormatterValueImpl
    )
