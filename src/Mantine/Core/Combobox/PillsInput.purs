module Mantine.Core.Combobox.PillsInput
  ( pillsInput
  , Props_PillsInput
  , Props_PillsInputImpl

  , pillsInputField
  , Props_PillsInputField
  , Props_PillsInputFieldImpl
  , PillsInputFieldType(..)
  , PillsInputFieldTypeImpl
  ) where

import Mantine.Core.Inputs.Input (Props_InputComponent, Props_InputComponentImpl)
import Mantine.Core.Prelude

pillsInput
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_PillsInput
  => Union attrsImpl attrsImpl_ Props_PillsInputImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
pillsInput = element (unsafeCoerce pillsInputComponent) <<< toNative

foreign import pillsInputComponent :: ReactComponent (Record Props_PillsInputImpl)

type Props_PillsInput     = Props_InputComponent     ( children :: Array JSX )
type Props_PillsInputImpl = Props_InputComponentImpl ( children :: Array JSX )

pillsInputField
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_PillsInputField
  => Union attrsImpl attrsImpl_ Props_PillsInputFieldImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
pillsInputField = element (unsafeCoerce pillsInputFieldComponent) <<< toNative

foreign import pillsInputFieldComponent :: ReactComponent (Record Props_PillsInputFieldImpl)

type Props_PillsInputField =
  Props_InputComponent
    ( type :: PillsInputFieldType
    | RawControlled String
    )

data PillsInputFieldType
  = PillsInputFieldTypeHidden
  | PillsInputFieldTypeAuto
  | PillsInputFieldTypeVisible

type PillsInputFieldTypeImpl = String

instance ToFFI PillsInputFieldType PillsInputFieldTypeImpl where
  toNative = case _ of
    PillsInputFieldTypeHidden  -> "hidden"
    PillsInputFieldTypeAuto    -> "auto"
    PillsInputFieldTypeVisible -> "visible"

type Props_PillsInputFieldImpl =
  Props_InputComponentImpl
    ( type :: PillsInputFieldTypeImpl
    | RawControlledImpl String
    )
