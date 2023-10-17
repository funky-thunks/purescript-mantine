module Mantine.Core.Layout.Group
  ( group
  , group_
  , GroupProps
  ) where

import Mantine.Core.Prelude

group :: (GroupProps -> GroupProps) -> JSX
group = mkComponentWithDefault groupComponent defaultGroupProps

group_ :: Array JSX -> JSX
group_ children = group _ { children = children }

foreign import groupComponent :: ReactComponent GroupPropsImpl

type GroupProps =
  ThemingProps
    ( children :: Array JSX
    , grow     :: Boolean
    , noWrap   :: Boolean
    , align    :: Maybe AlignItems
    , position :: Maybe Position
    , spacing  :: Maybe MantineNumberSize
    )

defaultGroupProps :: GroupProps
defaultGroupProps =
  defaultThemingProps
    { noWrap: true
    } `union` defaultValue

type GroupPropsImpl =
  ThemingPropsImpl
    ( children :: Array JSX
    , grow     :: Boolean
    , noWrap   :: Boolean
    , align    :: Nullable String
    , position :: Nullable String
    , spacing  :: Nullable MantineNumberSizeImpl
    )
