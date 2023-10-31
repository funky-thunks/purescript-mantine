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
    ( align    :: Maybe AlignItems
    , children :: Array JSX
    , gap      :: Maybe MantineSpacing
    , justify  :: Maybe JustifyContent
    )

type StackPropsImpl =
  MantineComponentImpl
    ( align    :: Nullable AlignItemsImpl
    , children :: Array JSX
    , gap      :: Nullable MantineSpacingImpl
    , justify  :: Nullable JustifyContentImpl
    )
