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
  ThemingProps
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
  ThemingPropsImpl
    ( align     :: Nullable String
    , children  :: Array JSX
    , columnGap :: Nullable String
    , direction :: Nullable String
    , gap       :: Nullable String
    , justify   :: Nullable String
    , rowGap    :: Nullable String
    , wrap      :: String
    )
