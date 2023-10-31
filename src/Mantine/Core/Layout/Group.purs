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
  ThemingProps
    ( align               :: Maybe AlignItems
    , children            :: Array JSX
    , gap                 :: Maybe MantineNumberSize
    , grow                :: Boolean
    , justify             :: Maybe JustifyContent
    , preventGrowOverflow :: Boolean
    , wrap                :: FlexWrap
    )

type GroupPropsImpl =
  ThemingPropsImpl
    ( align               :: Nullable String
    , children            :: Array JSX
    , gap                 :: Nullable MantineNumberSizeImpl
    , grow                :: Boolean
    , justify             :: Nullable String
    , preventGrowOverflow :: Boolean
    , wrap                :: String
    )
