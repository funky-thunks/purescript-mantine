module Mantine.Core.Inputs.Radio
  ( radio
  , RadioProps

  , radioGroup
  , radioGroup_
  , RadioGroupProps
  ) where

import Mantine.Core.Inputs.Checkables (CheckableComponent, CheckableComponentImpl)
import Mantine.Core.Inputs.Input (InputGroupComponent, InputGroupComponentImpl)
import Mantine.Core.Prelude

radio :: (RadioProps -> RadioProps) -> JSX
radio = mkTrivialComponent radioComponent

foreign import radioComponent :: ReactComponent RadioPropsImpl

type RadioProps =
  CheckableComponent
    ( defaultChecked :: Maybe Boolean
    , icon           :: Maybe JSX
    )

type RadioPropsImpl =
  CheckableComponentImpl
    ( defaultChecked :: Nullable Boolean
    , icon           :: Nullable JSX
    )

radioGroup :: (RadioGroupProps -> RadioGroupProps) -> JSX
radioGroup = mkTrivialComponent radioGroupComponent

radioGroup_ :: Array JSX -> JSX
radioGroup_ children = radioGroup _ { children = children }

foreign import radioGroupComponent :: ReactComponent RadioGroupPropsImpl

type RadioGroupProps     = InputGroupComponent     String (name :: Maybe    String)
type RadioGroupPropsImpl = InputGroupComponentImpl String (name :: Nullable String)
