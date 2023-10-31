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
    ( align     :: Maybe AlignItems
    , children  :: Array JSX
    , columnGap :: Maybe MantineSize
    , direction :: Maybe FlexDirection
    , gap       :: Maybe MantineSize
    , justify   :: Maybe JustifyContent
    , rowGap    :: Maybe MantineSize
    , wrap      :: FlexWrap
    )

type FlexPropsImpl =
  MantineComponentImpl
    ( align     :: Nullable AlignItemsImpl
    , children  :: Array JSX
    , columnGap :: Nullable MantineSizeImpl
    , direction :: Nullable FlexDirectionImpl
    , gap       :: Nullable MantineSizeImpl
    , justify   :: Nullable JustifyContentImpl
    , rowGap    :: Nullable MantineSizeImpl
    , wrap      :: FlexWrapImpl
    )
