module Mantine.Core.Inputs.Switch
  ( switch
  , SwitchProps
  , SwitchInnerLabels

  , switchGroup
  , switchGroup_
  , SwitchGroupProps
  ) where

import Mantine.Core.Inputs.Checkables (CheckableFieldComponent, CheckableFieldComponentImpl)
import Mantine.Core.Inputs.Input (InputGroupComponent, InputGroupComponentImpl)
import Mantine.Core.Prelude

switch :: (SwitchProps -> SwitchProps) -> JSX
switch = mkComponent switchComponent switchToImpl defaultMantineComponent_

foreign import switchComponent :: ReactComponent SwitchPropsImpl

type SwitchProps =
  CheckableFieldComponent
    ( innerLabels :: Optional SwitchInnerLabels
    , thumbIcon   :: Optional JSX
    )

type SwitchInnerLabels =
  { on  :: JSX
  , off :: JSX
  }

type SwitchPropsImpl =
  CheckableFieldComponentImpl
    ( offLabel  :: OptionalImpl JSX
    , onLabel   :: OptionalImpl JSX
    , thumbIcon :: OptionalImpl JSX
    )

switchToImpl :: SwitchProps -> SwitchPropsImpl
switchToImpl =
  let flattenLabels props =
        delete (Proxy :: Proxy "innerLabels") props `union`
          { offLabel: _.off <$> props.innerLabels
          , onLabel:  _.on  <$> props.innerLabels
          }
   in toNative <<< flattenLabels

switchGroup :: (SwitchGroupProps -> SwitchGroupProps) -> JSX
switchGroup = mkTrivialComponent switchGroupComponent

switchGroup_ :: Array JSX -> JSX
switchGroup_ children = switchGroup _ { children = children }

foreign import switchGroupComponent :: ReactComponent SwitchGroupPropsImpl

type SwitchGroupProps     = InputGroupComponent     (Array String) ()
type SwitchGroupPropsImpl = InputGroupComponentImpl (Array String) ()
