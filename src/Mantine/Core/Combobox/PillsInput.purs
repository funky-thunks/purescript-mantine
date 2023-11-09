module Mantine.Core.Combobox.PillsInput
  ( pillsInput
  , PillsInputProps

  , pillsInputField
  , PillsInputFieldProps
  , PillsInputFieldType(..)
  ) where

import Mantine.Core.Inputs.Input (InputComponent, InputComponentImpl)
import Mantine.Core.Prelude

pillsInput :: (PillsInputProps -> PillsInputProps) -> JSX
pillsInput = mkTrivialComponent pillsInputComponent

foreign import pillsInputComponent :: ReactComponent PillsInputPropsImpl

type PillsInputProps     = InputComponent     ()
type PillsInputPropsImpl = InputComponentImpl ()

pillsInputField :: (PillsInputFieldProps -> PillsInputFieldProps) -> JSX
pillsInputField = mkTrivialComponent pillsInputFieldComponent

foreign import pillsInputFieldComponent :: ReactComponent PillsInputFieldPropsImpl

type PillsInputFieldProps =
  MantineComponent
    ( placeholder :: Maybe String
    , pointer     :: Boolean
    , type        :: PillsInputFieldType
    )

data PillsInputFieldType
  = PillsInputFieldTypeHidden
  | PillsInputFieldTypeAuto
  | PillsInputFieldTypeVisible

instance DefaultValue PillsInputFieldType where
  defaultValue = PillsInputFieldTypeVisible

type PillsInputFieldTypeImpl = String

instance ToFFI PillsInputFieldType PillsInputFieldTypeImpl where
  toNative = case _ of
    PillsInputFieldTypeHidden  -> "hidden"
    PillsInputFieldTypeAuto    -> "auto"
    PillsInputFieldTypeVisible -> "visible"

type PillsInputFieldPropsImpl =
  MantineComponentImpl
    ( placeholder :: Nullable String
    , pointer     :: Boolean
    , type        :: PillsInputFieldTypeImpl
    )
