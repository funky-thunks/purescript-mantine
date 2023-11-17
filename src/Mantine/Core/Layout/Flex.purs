module Mantine.Core.Layout.Flex
  ( flex
  , flex_
  , FlexProps
  ) where

import Mantine.Core.Prelude

flex :: (FlexProps -> FlexProps) -> JSX
flex = mkTrivialComponent flexComponent

flex_ :: Array JSX -> JSX
flex_ children = flex _ { children = children }

foreign import flexComponent :: ReactComponent FlexPropsImpl

type FlexProps =
  MantineComponent
    ( align     :: Optional AlignItems
    , children  :: Array JSX
    , columnGap :: Optional MantineSize
    , direction :: Optional FlexDirection
    , gap       :: Optional MantineSize
    , justify   :: Optional JustifyContent
    , rowGap    :: Optional MantineSize
    , wrap      :: FlexWrap
    )

type FlexPropsImpl =
  MantineComponentImpl
    ( align     :: OptionalImpl AlignItemsImpl
    , children  :: Array JSX
    , columnGap :: OptionalImpl MantineSizeImpl
    , direction :: OptionalImpl FlexDirectionImpl
    , gap       :: OptionalImpl MantineSizeImpl
    , justify   :: OptionalImpl JustifyContentImpl
    , rowGap    :: OptionalImpl MantineSizeImpl
    , wrap      :: FlexWrapImpl
    )
