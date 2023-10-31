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
  MantineComponent
    ( center        :: Boolean
    , children      :: Array JSX
    , icon          :: Maybe JSX
    , listStyleType :: Maybe ListStyleType
    , size          :: Maybe MantineNumberSize
    , spacing       :: Maybe MantineNumberSize
    , type          :: ListType
    , withPadding   :: Boolean
    )

data ListType
  = ListTypeOrdered
  | ListTypeUnordered

instance DefaultValue ListType where defaultValue = ListTypeUnordered

type ListTypeImpl = String

instance ToFFI ListType ListTypeImpl where
  toNative = case _ of
    ListTypeOrdered   -> "ordered"
    ListTypeUnordered -> "unordered"

type ListPropsImpl =
  MantineComponentImpl
    ( center        :: Boolean
    , children      :: Array JSX
    , icon          :: Nullable JSX
    , listStyleType :: Nullable ListStyleTypeImpl
    , size          :: Nullable MantineNumberSizeImpl
    , spacing       :: Nullable MantineNumberSizeImpl
    , type          :: ListTypeImpl
    , withPadding   :: Boolean
    )

listItem :: (ListItemProps -> ListItemProps) -> JSX
listItem = mkTrivialComponent listItemComponent

listItem_ :: Array JSX -> JSX
listItem_ children = listItem _ { children = children }

foreign import listItemComponent :: ReactComponent ListItemPropsImpl

type ListItemProps =
  MantineComponent
    ( children :: Array JSX
    , icon     :: Maybe JSX
    )

type ListItemPropsImpl =
  MantineComponentImpl
    ( children :: Array JSX
    , icon     :: Nullable JSX
    )
