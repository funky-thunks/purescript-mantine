module Mantine.Core.Layout.Group
  ( group
  , group_
  , GroupProps
  ) where

import Mantine.Core.Prelude

group :: (GroupProps -> GroupProps) -> JSX
group = mkTrivialComponent groupComponent

group_ :: Array JSX -> JSX
group_ children = group _ { children = children }

foreign import groupComponent :: ReactComponent GroupPropsImpl

type GroupProps =
  MantineComponent
    ( align               :: Optional AlignItems
    , children            :: Array JSX
    , gap                 :: Optional MantineSpacing
    , grow                :: Boolean
    , justify             :: Optional JustifyContent
    , preventGrowOverflow :: Boolean
    , wrap                :: FlexWrap
    )

type GroupPropsImpl =
  MantineComponentImpl
    ( align               :: OptionalImpl AlignItemsImpl
    , children            :: Array JSX
    , gap                 :: OptionalImpl MantineSpacingImpl
    , grow                :: Boolean
    , justify             :: OptionalImpl JustifyContentImpl
    , preventGrowOverflow :: Boolean
    , wrap                :: FlexWrapImpl
    )
