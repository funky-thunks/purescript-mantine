module Mantine.Core.DataDisplay.NumberFormatter
  ( numberFormatter
  , Props_NumberFormatter
  , Props_NumberFormatterImpl
  , NumberFormatterValue(..)
  , NumberFormatterValueImpl
  ) where

import Mantine.Core.Inputs.NumberInput (ThousandsGroupStyle, ThousandsGroupStyleImpl, ThousandSeparator, ThousandSeparatorImpl)
import Mantine.Core.Prelude

numberFormatter
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_NumberFormatter
  => Union attrsImpl attrsImpl_ Props_NumberFormatterImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
numberFormatter = element (unsafeCoerce numberFormatterComponent) <<< toNative

foreign import numberFormatterComponent :: ReactComponent (Record Props_NumberFormatterImpl)

type Props_NumberFormatter =
  Props_Common
    ( allowNegative       :: Boolean
    , decimalScale        :: Number
    , decimalSeparator    :: String
    , fixedDecimalScale   :: Boolean
    , prefix              :: String
    , suffix              :: String
    , thousandSeparator   :: ThousandSeparator
    , thousandsGroupStyle :: ThousandsGroupStyle
    , value               :: NumberFormatterValue
    )

data NumberFormatterValue
  = AsNumber Number
  | AsString String

type NumberFormatterValueImpl = String |+| Number

instance ToFFI NumberFormatterValue NumberFormatterValueImpl where
  toNative = case _ of
    AsNumber n -> asOneOf n
    AsString s -> asOneOf s

type Props_NumberFormatterImpl =
  Props_CommonImpl
    ( allowNegative       :: Boolean
    , decimalScale        :: Number
    , decimalSeparator    :: String
    , fixedDecimalScale   :: Boolean
    , prefix              :: String
    , suffix              :: String
    , thousandSeparator   :: ThousandSeparatorImpl
    , thousandsGroupStyle :: ThousandsGroupStyleImpl
    , value               :: NumberFormatterValueImpl
    )
