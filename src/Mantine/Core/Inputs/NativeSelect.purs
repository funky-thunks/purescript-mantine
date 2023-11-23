module Mantine.Core.Inputs.NativeSelect
  ( nativeSelect
  , Props_NativeSelect
  , Props_NativeSelectImpl
  ) where

import Mantine.Core.Combobox.Select (SelectItem, SelectItemImpl)
import Mantine.Core.Inputs.Input (Props_InputComponent, Props_InputComponentImpl)
import Mantine.Core.Prelude

nativeSelect
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_NativeSelect
  => Union attrsImpl attrsImpl_ Props_NativeSelectImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
nativeSelect = element (unsafeCoerce nativeSelectComponent) <<< toNative

foreign import nativeSelectComponent :: ReactComponent (Record Props_NativeSelectImpl)

type Props_NativeSelect =
  Props_InputComponent
    ( data :: Array SelectItem
    | Controlled String
    )

type Props_NativeSelectImpl =
  Props_InputComponentImpl
    ( data :: Array SelectItemImpl
    | ControlledImpl String
    )
