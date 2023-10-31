module Mantine.Core.Inputs.Checkbox
  ( checkbox
  , CheckboxProps

  , checkboxGroup
  , checkboxGroup_
  , CheckboxGroupProps
  ) where

import Mantine.Core.Inputs.Checkables (CheckableComponent, CheckableComponentImpl)
import Mantine.Core.Inputs.Input (InputGroupComponent, InputGroupComponentImpl)
import Mantine.Core.Prelude

checkbox :: (CheckboxProps -> CheckboxProps) -> JSX
checkbox = mkComponent checkboxComponent toNativeCheckbox defaultThemingProps_

foreign import checkboxComponent :: ReactComponent CheckboxPropsImpl

type CheckboxProps =
  CheckableComponent
    ( disabled      :: Boolean
    , icon          :: Maybe ({ indeterminate :: Boolean, className :: String } -> JSX)
    , indeterminate :: Maybe Boolean
    )

type CheckboxPropsImpl =
  CheckableComponentImpl
    ( disabled      :: Boolean
    , icon          :: Nullable ({ indeterminate :: Boolean, className :: String } -> JSX)
    , indeterminate :: Nullable Boolean
    )

toNativeCheckbox :: CheckboxProps -> CheckboxPropsImpl
toNativeCheckbox props =
  let rest = toNative <<< delete (Proxy :: Proxy "icon")
   in { icon: toNullable props.icon } `union` rest props

checkboxGroup :: (CheckboxGroupProps -> CheckboxGroupProps) -> JSX
checkboxGroup = mkTrivialComponent checkboxGroupComponent

checkboxGroup_ :: Array JSX -> JSX
checkboxGroup_ children = checkboxGroup _ { children = children }

foreign import checkboxGroupComponent :: ReactComponent CheckboxGroupPropsImpl

type CheckboxGroupProps     = InputGroupComponent     (Array String) ()
type CheckboxGroupPropsImpl = InputGroupComponentImpl (Array String) ()
