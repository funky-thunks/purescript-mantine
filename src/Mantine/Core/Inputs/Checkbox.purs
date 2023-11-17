module Mantine.Core.Inputs.Checkbox
  ( checkbox
  , Props_Checkbox
  , Props_CheckboxImpl

  , checkboxGroup
  , checkboxGroup_
  , Props_CheckboxGroup
  , Props_CheckboxGroupImpl
  ) where

import Mantine.Core.Inputs.Checkables (Props_CheckableFieldComponent, Props_CheckableFieldComponentImpl)
import Mantine.Core.Inputs.Input (Props_InputGroupComponent, Props_InputGroupComponentImpl)
import Mantine.Core.Prelude

checkbox
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Checkbox
  => Union attrsImpl attrsImpl_ Props_CheckboxImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
checkbox = element (unsafeCoerce checkboxComponent) <<< toNative

foreign import checkboxComponent :: ReactComponent (Record Props_CheckboxImpl)

type Props_Checkbox =
  Props_CheckableFieldComponent
    ( disabled      :: Boolean
    , icon          :: { indeterminate :: Boolean, className :: String } -> JSX
    , iconColor     :: MantineColor
    , indeterminate :: Boolean
    )

type Props_CheckboxImpl =
  Props_CheckableFieldComponentImpl
    ( disabled      :: Boolean
    , icon          :: { indeterminate :: Boolean, className :: String } -> JSX
    , iconColor     :: MantineColorImpl
    , indeterminate :: Boolean
    )

checkboxGroup
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_CheckboxGroup
  => Union attrsImpl attrsImpl_ Props_CheckboxGroupImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
checkboxGroup = element (unsafeCoerce checkboxGroupComponent) <<< toNative

checkboxGroup_ :: Array JSX -> JSX
checkboxGroup_ children = checkboxGroup { children }

foreign import checkboxGroupComponent :: ReactComponent (Record Props_CheckboxGroupImpl)

type Props_CheckboxGroup     = Props_InputGroupComponent     (Array String) ()
type Props_CheckboxGroupImpl = Props_InputGroupComponentImpl (Array String) ()
