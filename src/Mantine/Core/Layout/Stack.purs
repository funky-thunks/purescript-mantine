module Mantine.Core.Layout.Stack
  ( stack
  , stack_
  , StackProps
  ) where

import Mantine.Core.Prelude

stack :: (StackProps -> StackProps) -> JSX
stack = mkTrivialComponent stackComponent

stack_ :: Array JSX -> JSX
stack_ children = stack _ { children = children }

foreign import stackComponent :: ReactComponent StackPropsImpl

type StackProps =
  MantineComponent
    ( align    :: Optional AlignItems
    , children :: Array JSX
    , gap      :: Optional MantineSpacing
    , justify  :: Optional JustifyContent
    )

type StackPropsImpl =
  MantineComponentImpl
    ( align    :: OptionalImpl AlignItemsImpl
    , children :: Array JSX
    , gap      :: OptionalImpl MantineSpacingImpl
    , justify  :: OptionalImpl JustifyContentImpl
    )
