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
    , cols            :: Optional (FixedOrResponsive Int)
    , spacing         :: Optional (FixedOrResponsive MantineSpacing)
    , verticalSpacing :: Optional (FixedOrResponsive MantineSpacing)
    )

type SimpleGridPropsImpl =
  MantineComponentImpl
    ( children        :: Array JSX
    , cols            :: OptionalImpl (FixedOrResponsiveImpl Number)
    , spacing         :: OptionalImpl (FixedOrResponsiveImpl MantineSpacingImpl)
    , verticalSpacing :: OptionalImpl (FixedOrResponsiveImpl MantineSpacingImpl)
    )
