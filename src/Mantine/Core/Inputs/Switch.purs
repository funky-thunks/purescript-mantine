module Mantine.Core.Inputs.Switch
  ( switch
  , SwitchProps
  , SwitchInnerLabels

  , switchGroup
  , switchGroup_
  , SwitchGroupProps
  ) where

import Mantine.Core.Inputs.Checkables (CheckableComponent, CheckableComponentImpl)
import Mantine.Core.Inputs.Input (InputGroupComponent, InputGroupComponentImpl)
import Mantine.Core.Prelude

switch :: (SwitchProps -> SwitchProps) -> JSX
switch = mkComponent switchComponent switchToImpl defaultThemingProps_

foreign import switchComponent :: ReactComponent SwitchPropsImpl

type SwitchProps =
  CheckableComponent
    ( innerLabels :: Maybe SwitchInnerLabels
    , thumbIcon   :: Maybe JSX
    )

type SwitchInnerLabels =
  { on  :: JSX
  , off :: JSX
  }

type SwitchPropsImpl =
  CheckableComponentImpl
    ( offLabel  :: Nullable JSX
    , onLabel   :: Nullable JSX
    , thumbIcon :: Nullable JSX
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
