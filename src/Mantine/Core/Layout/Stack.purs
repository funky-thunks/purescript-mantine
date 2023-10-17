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
  ThemingProps
    ( align    :: Maybe AlignItems
    , children :: Array JSX
    , justify  :: Maybe JustifyContent
    , spacing  :: Maybe MantineNumberSize
    )

type StackPropsImpl =
  ThemingPropsImpl
    ( align    :: Nullable String
    , children :: Array JSX
    , justify  :: Nullable String
    , spacing  :: Nullable MantineNumberSizeImpl
    )
