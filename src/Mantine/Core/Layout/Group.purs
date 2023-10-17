module Mantine.Core.Layout.Group
  ( group
  , group_
  , GroupProps

  , module Mantine.Core.Common
  ) where

import Data.Default (defaultValue)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Mantine.Core.Common (AlignItems, MantineNumberSize, Position)
import Mantine.Core.Common as MC
import Mantine.FFI (toNative)
import React.Basic (ReactComponent, element)
import React.Basic.Hooks (JSX)
import Record (union)

group :: (GroupProps -> GroupProps) -> JSX
group setProps = element groupComponent (toNative (setProps defaultGroupProps))

group_ :: Array JSX -> JSX
group_ children = group _ { children = children }

foreign import groupComponent :: ReactComponent GroupPropsImpl

type GroupProps =
  MC.ThemingProps
    ( children :: Array JSX
    , grow     :: Boolean
    , noWrap   :: Boolean
    , align    :: Maybe AlignItems
    , position :: Maybe Position
    , spacing  :: Maybe MantineNumberSize
    )

defaultGroupProps :: GroupProps
defaultGroupProps =
  MC.defaultThemingProps
    { noWrap: true
    } `union` defaultValue

type GroupPropsImpl =
  MC.ThemingPropsImpl
    ( children :: Array JSX
    , grow     :: Boolean
    , noWrap   :: Boolean
    , align    :: Nullable String
    , position :: Nullable String
    , spacing  :: Nullable MC.MantineNumberSizeImpl
    )
