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
  ThemingProps
    ( children :: Array JSX
    , legend   :: Maybe JSX
    , radius   :: Maybe MantineNumberSize
    , variant  :: FieldsetVariant
    )

data FieldsetVariant
  = FieldsetVariantDefault
  | FieldsetVariantFilled
  | FieldsetVariantUnstyled

instance DefaultValue FieldsetVariant where
  defaultValue = FieldsetVariantDefault

instance ToFFI FieldsetVariant (Nullable String) where
  toNative = toNative <<< case _ of
    FieldsetVariantDefault  -> Nothing
    FieldsetVariantFilled   -> Just "filled"
    FieldsetVariantUnstyled -> Just "unstyled"

type FieldsetPropsImpl =
  ThemingPropsImpl
    ( children :: Array JSX
    , legend   :: Nullable JSX
    , radius   :: Nullable MantineNumberSizeImpl
    , variant  :: Nullable String
    )
