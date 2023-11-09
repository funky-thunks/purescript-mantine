module Mantine.Core.Layout.SimpleGrid
  ( simpleGrid
  , simpleGrid_
  , SimpleGridProps
  ) where

import Mantine.Core.Prelude

simpleGrid :: (SimpleGridProps -> SimpleGridProps) -> JSX
simpleGrid = mkTrivialComponent simpleGridComponent

simpleGrid_ :: Array JSX -> JSX
simpleGrid_ children = simpleGrid _ { children = children }

foreign import simpleGridComponent :: ReactComponent SimpleGridPropsImpl

type SimpleGridProps =
  MantineComponent
    ( children        :: Array JSX
    , cols            :: Maybe (FixedOrResponsive Int)
    , spacing         :: Maybe (FixedOrResponsive MantineSpacing)
    , verticalSpacing :: Maybe (FixedOrResponsive MantineSpacing)
    )

type SimpleGridPropsImpl =
  MantineComponentImpl
    ( children        :: Array JSX
    , cols            :: Nullable (FixedOrResponsiveImpl Number)
    , spacing         :: Nullable (FixedOrResponsiveImpl MantineSpacingImpl)
    , verticalSpacing :: Nullable (FixedOrResponsiveImpl MantineSpacingImpl)
    )
