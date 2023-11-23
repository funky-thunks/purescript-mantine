module Mantine.Core.Layout.SimpleGrid
  ( simpleGrid
  , simpleGrid_
  , Props_SimpleGrid
  , Props_SimpleGridImpl
  ) where

import Mantine.Core.Prelude

simpleGrid
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_SimpleGrid
  => Union attrsImpl attrsImpl_ Props_SimpleGridImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
simpleGrid = element (unsafeCoerce simpleGridComponent) <<< toNative

simpleGrid_ :: Array JSX -> JSX
simpleGrid_ children = simpleGrid { children }

foreign import simpleGridComponent :: ReactComponent (Record Props_SimpleGridImpl)

type Props_SimpleGrid =
  Props_Common
    ( children        :: Array JSX
    , cols            :: FixedOrResponsive Int
    , spacing         :: FixedOrResponsive MantineSpacing
    , verticalSpacing :: FixedOrResponsive MantineSpacing
    )

type Props_SimpleGridImpl =
  Props_CommonImpl
    ( children        :: Array JSX
    , cols            :: FixedOrResponsiveImpl Number
    , spacing         :: FixedOrResponsiveImpl MantineSpacingImpl
    , verticalSpacing :: FixedOrResponsiveImpl MantineSpacingImpl
    )
