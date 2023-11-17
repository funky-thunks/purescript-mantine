module Mantine.Core.Inputs.Radio
  ( radio
  , RadioProps

  , radioGroup
  , radioGroup_
  , RadioGroupProps
  ) where

import Mantine.Core.Inputs.Checkables (CheckableFieldComponent, CheckableFieldComponentImpl)
import Mantine.Core.Inputs.Input (InputGroupComponent, InputGroupComponentImpl)
import Mantine.Core.Prelude

radio :: (RadioProps -> RadioProps) -> JSX
radio = mkTrivialComponent radioComponent

foreign import radioComponent :: ReactComponent RadioPropsImpl

type RadioProps =
  CheckableFieldComponent
    ( icon      :: Optional JSX
    , iconColor :: Optional MantineColor
    )

type RadioPropsImpl =
  CheckableFieldComponentImpl
    ( icon      :: OptionalImpl JSX
    , iconColor :: OptionalImpl MantineColorImpl
    )

radioGroup :: (RadioGroupProps -> RadioGroupProps) -> JSX
radioGroup = mkTrivialComponent radioGroupComponent

radioGroup_ :: Array JSX -> JSX
radioGroup_ children = radioGroup _ { children = children }

foreign import radioGroupComponent :: ReactComponent RadioGroupPropsImpl

type RadioGroupProps     = InputGroupComponent     String (name :: Optional     String)
type RadioGroupPropsImpl = InputGroupComponentImpl String (name :: OptionalImpl String)
