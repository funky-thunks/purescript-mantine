module Mantine.Core.Layout.SimpleGrid
  ( simpleGrid
  , simpleGrid_
  , SimpleGridProps
  , SimpleGridBreakpoint(..)
  ) where

import Mantine.Core.Prelude

simpleGrid :: (SimpleGridProps -> SimpleGridProps) -> JSX
simpleGrid = mkComponentWithDefault simpleGridComponent defaultThemingProps_

simpleGrid_ :: Array JSX -> JSX
simpleGrid_ children = simpleGrid _ { children = children }

type SimpleGridProps =
  ThemingProps
    ( breakpoints     :: Array SimpleGridBreakpoint
    , children        :: Array JSX
    , cols            :: Maybe Int
    , spacing         :: Maybe MantineSize
    , verticalSpacing :: Maybe MantineSize
    )

data SimpleGridBreakpoint
  = SimpleGridBreakpointMaxWidth { width :: Number, cols :: Maybe Int, spacing :: Maybe MantineSize }
  | SimpleGridBreakpointMinWidth { width :: Number, cols :: Maybe Int, spacing :: Maybe MantineSize }

type SimpleGridBreakpointImpl = { maxWidth :: Number, cols :: Nullable Number, spacing :: Nullable String }
                            |+| { minWidth :: Number, cols :: Nullable Number, spacing :: Nullable String }

instance ToFFI SimpleGridBreakpoint SimpleGridBreakpointImpl where
  toNative = case _ of
    SimpleGridBreakpointMaxWidth { width, cols, spacing } -> asOneOf (toNative { maxWidth: width, cols, spacing })
    SimpleGridBreakpointMinWidth { width, cols, spacing } -> asOneOf (toNative { minWidth: width, cols, spacing })

type SimpleGridPropsImpl =
  ThemingPropsImpl
    ( breakpoints     :: Array SimpleGridBreakpointImpl
    , children        :: Array JSX
    , cols            :: Nullable Number
    , spacing         :: Nullable String
    , verticalSpacing :: Nullable String
    )

foreign import simpleGridComponent :: ReactComponent SimpleGridPropsImpl
