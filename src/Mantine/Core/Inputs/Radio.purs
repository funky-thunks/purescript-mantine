module Mantine.Core.Inputs.Radio
  ( radio
  , Props_Radio
  , Props_RadioImpl

  , radioGroup
  , radioGroup_
  , Props_RadioGroup
  , Props_RadioGroupImpl
  ) where

import Mantine.Core.Inputs.Checkables (Props_CheckableFieldComponent, Props_CheckableFieldComponentImpl)
import Mantine.Core.Inputs.Input (Props_InputGroupComponent, Props_InputGroupComponentImpl)
import Mantine.Core.Prelude

radio
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Radio
  => Union attrsImpl attrsImpl_ Props_RadioImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
radio = element (unsafeCoerce radioComponent) <<< toNative

foreign import radioComponent :: ReactComponent (Record Props_RadioImpl)

type Props_Radio =
  Props_CheckableFieldComponent
    ( icon      :: JSX
    , iconColor :: MantineColor
    )

type Props_RadioImpl =
  Props_CheckableFieldComponentImpl
    ( icon      :: JSX
    , iconColor :: MantineColorImpl
    )

radioGroup
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_RadioGroup
  => Union attrsImpl attrsImpl_ Props_RadioGroupImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
radioGroup = element (unsafeCoerce radioGroupComponent) <<< toNative

radioGroup_ :: Array JSX -> JSX
radioGroup_ children = radioGroup { children }

foreign import radioGroupComponent :: ReactComponent (Record Props_RadioGroupImpl)

type Props_RadioGroup     = Props_InputGroupComponent     String (name :: String)
type Props_RadioGroupImpl = Props_InputGroupComponentImpl String (name :: String)
