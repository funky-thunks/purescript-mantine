module Mantine.Core.Typography.List
  ( list
  , list_
  , ListProps
  , ListType(..)

  , listItem
  , listItem_
  , ListItemProps
  ) where

import Mantine.Core.Prelude

list :: (ListProps -> ListProps) -> JSX
list = mkTrivialComponent listComponent

list_ :: Array JSX -> JSX
list_ children = list _ { children = children }

foreign import listComponent :: ReactComponent ListPropsImpl

type ListProps =
  ThemingProps
    ( children      :: Array JSX
    , center        :: Boolean
    , icon          :: Maybe JSX
    , listStyleType :: Maybe String
    , size          :: Maybe MantineNumberSize
    , spacing       :: Maybe MantineNumberSize
    , type          :: ListType
    , withPadding   :: Boolean
    )

data ListType
  = ListTypeOrdered
  | ListTypeUnordered

instance DefaultValue ListType where defaultValue = ListTypeUnordered

instance ToFFI ListType String where
  toNative = case _ of
    ListTypeOrdered   -> "ordered"
    ListTypeUnordered -> "unordered"

type ListPropsImpl =
  ThemingPropsImpl
    ( children      :: Array JSX
    , center        :: Boolean
    , icon          :: Nullable JSX
    , listStyleType :: Nullable String
    , size          :: Nullable MantineNumberSizeImpl
    , spacing       :: Nullable MantineNumberSizeImpl
    , type          :: String
    , withPadding   :: Boolean
    )

listItem :: (ListItemProps -> ListItemProps) -> JSX
listItem = mkTrivialComponent listItemComponent

listItem_ :: Array JSX -> JSX
listItem_ children = listItem _ { children = children }

foreign import listItemComponent :: ReactComponent ListItemPropsImpl

type ListItemProps =
  ThemingProps
    ( children :: Array JSX
    , icon     :: Maybe JSX
    )

type ListItemPropsImpl =
  ThemingPropsImpl
    ( children :: Array JSX
    , icon     :: Nullable JSX
    )
