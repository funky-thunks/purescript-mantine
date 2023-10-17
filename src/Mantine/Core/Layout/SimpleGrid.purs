module Mantine.Core.Layout.SimpleGrid
  ( simpleGrid
  , simpleGrid_
  , SimpleGridProps
  , SimpleGridBreakpoint(..)
  ) where

import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Mantine.Core.Common as MC
import Mantine.FFI (class ToFFI, toNative)
import React.Basic (ReactComponent, element)
import React.Basic.Hooks (JSX)
import Untagged.Union (type (|+|), asOneOf)

simpleGrid :: (SimpleGridProps -> SimpleGridProps) -> JSX
simpleGrid setProps = element simpleGridComponent (toNative (setProps MC.defaultThemingProps_))

simpleGrid_ :: Array JSX -> JSX
simpleGrid_ children = simpleGrid _ { children = children }

type SimpleGridProps =
  MC.ThemingProps
    ( breakpoints     :: Array SimpleGridBreakpoint
    , children        :: Array JSX
    , cols            :: Maybe Int
    , spacing         :: Maybe MC.MantineSize
    , verticalSpacing :: Maybe MC.MantineSize
    )

data SimpleGridBreakpoint
  = SimpleGridBreakpointMaxWidth { width :: Number, cols :: Maybe Int, spacing :: Maybe MC.MantineSize }
  | SimpleGridBreakpointMinWidth { width :: Number, cols :: Maybe Int, spacing :: Maybe MC.MantineSize }

type SimpleGridBreakpointImpl = { maxWidth :: Number, cols :: Nullable Number, spacing :: Nullable String }
                            |+| { minWidth :: Number, cols :: Nullable Number, spacing :: Nullable String }

instance ToFFI SimpleGridBreakpoint SimpleGridBreakpointImpl where
  toNative = case _ of
    SimpleGridBreakpointMaxWidth { width, cols, spacing } -> asOneOf (toNative { maxWidth: width, cols, spacing })
    SimpleGridBreakpointMinWidth { width, cols, spacing } -> asOneOf (toNative { minWidth: width, cols, spacing })

type SimpleGridPropsImpl =
  MC.ThemingPropsImpl
    ( breakpoints     :: Array SimpleGridBreakpointImpl
    , children        :: Array JSX
    , cols            :: Nullable Number
    , spacing         :: Nullable String
    , verticalSpacing :: Nullable String
    )

foreign import simpleGridComponent :: ReactComponent SimpleGridPropsImpl
