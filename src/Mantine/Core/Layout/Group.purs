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
    ( align               :: Maybe AlignItems
    , children            :: Array JSX
    , gap                 :: Maybe MantineSpacing
    , grow                :: Boolean
    , justify             :: Maybe JustifyContent
    , preventGrowOverflow :: Boolean
    , wrap                :: FlexWrap
    )

type GroupPropsImpl =
  MantineComponentImpl
    ( align               :: Nullable AlignItemsImpl
    , children            :: Array JSX
    , gap                 :: Nullable MantineSpacingImpl
    , grow                :: Boolean
    , justify             :: Nullable JustifyContentImpl
    , preventGrowOverflow :: Boolean
    , wrap                :: FlexWrapImpl
    )
