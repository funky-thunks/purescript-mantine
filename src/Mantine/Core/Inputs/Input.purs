module Mantine.Core.Inputs.Input
  ( InputVariant(..)
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
