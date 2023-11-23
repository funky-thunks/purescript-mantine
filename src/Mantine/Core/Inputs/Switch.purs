module Mantine.Core.Inputs.Switch
  ( switch
  , Props_Switch
  , Props_SwitchImpl

  , switchGroup
  , switchGroup_
  , Props_SwitchGroup
  , Props_SwitchGroupImpl
  ) where

import Mantine.Core.Inputs.Checkables (Props_CheckableFieldComponent, Props_CheckableFieldComponentImpl)
import Mantine.Core.Inputs.Input (Props_InputGroupComponent, Props_InputGroupComponentImpl)
import Mantine.Core.Prelude

switch
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Switch
  => Union attrsImpl attrsImpl_ Props_SwitchImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
switch = element (unsafeCoerce switchComponent) <<< toNative

foreign import switchComponent :: ReactComponent (Record Props_SwitchImpl)

type Props_Switch =
  Props_CheckableFieldComponent
    ( offLabel  :: JSX
    , onLabel   :: JSX
    , thumbIcon :: JSX
    )

type Props_SwitchImpl =
  Props_CheckableFieldComponentImpl
    ( offLabel  :: JSX
    , onLabel   :: JSX
    , thumbIcon :: JSX
    )

switchGroup
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_SwitchGroup
  => Union attrsImpl attrsImpl_ Props_SwitchGroupImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
switchGroup = element (unsafeCoerce switchGroupComponent) <<< toNative

switchGroup_ :: Array JSX -> JSX
switchGroup_ children = switchGroup { children }

foreign import switchGroupComponent :: ReactComponent (Record Props_SwitchGroupImpl)

type Props_SwitchGroup     = Props_InputGroupComponent     (Array String) ()
type Props_SwitchGroupImpl = Props_InputGroupComponentImpl (Array String) ()
