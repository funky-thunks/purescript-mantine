module Mantine.Core.Inputs.FieldSet
  ( fieldset
  , Props_Fieldset
  , Props_FieldsetImpl
  , FieldsetVariant(..)
  , FieldsetVariantImpl
  ) where

import Mantine.Core.Prelude

fieldset
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Fieldset
  => Union attrsImpl attrsImpl_ Props_FieldsetImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
fieldset = element (unsafeCoerce fieldsetComponent) <<< toNative

foreign import fieldsetComponent :: ReactComponent (Record Props_FieldsetImpl)

type Props_Fieldset =
  Props_Common
    ( children :: Array JSX
    , legend   :: JSX
    , radius   :: MantineNumberSize
    , variant  :: FieldsetVariant
    )

data FieldsetVariant
  = FieldsetVariantDefault
  | FieldsetVariantFilled
  | FieldsetVariantUnstyled

type FieldsetVariantImpl = OptionalImpl String

instance ToFFI FieldsetVariant FieldsetVariantImpl where
  toNative = toNative <<< Optional <<< case _ of
    FieldsetVariantDefault  -> Nothing
    FieldsetVariantFilled   -> Just "filled"
    FieldsetVariantUnstyled -> Just "unstyled"

type Props_FieldsetImpl =
  Props_CommonImpl
    ( children :: Array JSX
    , legend   :: JSX
    , radius   :: MantineNumberSizeImpl
    , variant  :: FieldsetVariantImpl
    )
