module Mantine.Core.Inputs.Input
  ( InputVariant(..)
  , InputWrapperOrder(..)
  ) where

import Mantine.Core.Prelude

data InputVariant
  = InputVariantDefault
  | InputVariantUnstyled
  | InputVariantFilled

instance DefaultValue InputVariant where defaultValue = InputVariantDefault

instance ToFFI InputVariant String where
  toNative = case _ of
    InputVariantDefault  -> "default"
    InputVariantUnstyled -> "unstyled"
    InputVariantFilled   -> "filled"

data InputWrapperOrder
  = InputWrapperOrderInput
  | InputWrapperOrderLabel
  | InputWrapperOrderError
  | InputWrapperOrderDescription

instance ToFFI InputWrapperOrder String where
  toNative = case _ of
    InputWrapperOrderInput       -> "input"
    InputWrapperOrderLabel       -> "label"
    InputWrapperOrderError       -> "error"
    InputWrapperOrderDescription -> "description"
