module Mantine.Core.Layout.Stack
  ( stack
  , stack_
  , StackProps

  , module Mantine.Core.Common
  ) where

import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Mantine.Core.Common (AlignItems, JustifyContent, MantineNumberSize)
import Mantine.Core.Common as MC
import Mantine.FFI (toNative)
import React.Basic (ReactComponent, element)
import React.Basic.Hooks (JSX)

stack :: (StackProps -> StackProps) -> JSX
stack setProps = element stackComponent (toNative (setProps MC.defaultThemingProps_))

stack_ :: Array JSX -> JSX
stack_ children = stack _ { children = children }

foreign import stackComponent :: ReactComponent StackPropsImpl

type StackProps =
  MC.ThemingProps
    ( align    :: Maybe AlignItems
    , children :: Array JSX
    , justify  :: Maybe JustifyContent
    , spacing  :: Maybe MantineNumberSize
    )

type StackPropsImpl =
  MC.ThemingPropsImpl
    ( align    :: Nullable String
    , children :: Array JSX
    , justify  :: Nullable String
    , spacing  :: Nullable MC.MantineNumberSizeImpl
    )
