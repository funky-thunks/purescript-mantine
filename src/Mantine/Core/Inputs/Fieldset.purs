module Mantine.Core.Inputs.FieldSet
  ( fieldset
  , FieldsetProps
  , FieldsetVariant(..)
  ) where

import Mantine.Core.Prelude

fieldset :: (FieldsetProps -> FieldsetProps) -> JSX
fieldset = mkTrivialComponent fieldsetComponent

foreign import fieldsetComponent :: ReactComponent FieldsetPropsImpl

type FieldsetProps =
  MantineComponent
    ( children :: Array JSX
    , legend   :: Optional JSX
    , radius   :: Optional MantineNumberSize
    , variant  :: FieldsetVariant
    )

data FieldsetVariant
  = FieldsetVariantDefault
  | FieldsetVariantFilled
  | FieldsetVariantUnstyled

instance DefaultValue FieldsetVariant where
  defaultValue = FieldsetVariantDefault

type FieldsetVariantImpl = OptionalImpl String

instance ToFFI FieldsetVariant FieldsetVariantImpl where
  toNative = toNative <<< Optional <<< case _ of
    FieldsetVariantDefault  -> Nothing
    FieldsetVariantFilled   -> Just "filled"
    FieldsetVariantUnstyled -> Just "unstyled"

type FieldsetPropsImpl =
  MantineComponentImpl
    ( children :: Array JSX
    , legend   :: OptionalImpl JSX
    , radius   :: OptionalImpl MantineNumberSizeImpl
    , variant  :: FieldsetVariantImpl
    )
