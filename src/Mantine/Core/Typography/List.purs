module Mantine.Core.Typography.List
  ( list
  , list_
  , Props_List
  , Props_ListImpl
  , ListType(..)
  , ListTypeImpl

  , listItem
  , listItem_
  , Props_ListItem
  , Props_ListItemImpl
  ) where

import Mantine.Core.Prelude

list
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_List
  => Union attrsImpl attrsImpl_ Props_ListImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
list = element (unsafeCoerce listComponent) <<< toNative

list_ :: Array JSX -> JSX
list_ children = list { children }

foreign import listComponent :: ReactComponent (Record Props_ListImpl)

type Props_List =
  Props_Common
    ( center        :: Boolean
    , children      :: Array JSX
    , icon          :: JSX
    , listStyleType :: ListStyleType
    , size          :: MantineNumberSize
    , spacing       :: MantineNumberSize
    , type          :: ListType
    , withPadding   :: Boolean
    )

data ListType
  = ListTypeOrdered
  | ListTypeUnordered

type ListTypeImpl = String

instance ToFFI ListType ListTypeImpl where
  toNative = case _ of
    ListTypeOrdered   -> "ordered"
    ListTypeUnordered -> "unordered"

type Props_ListImpl =
  Props_CommonImpl
    ( center        :: Boolean
    , children      :: Array JSX
    , icon          :: JSX
    , listStyleType :: ListStyleTypeImpl
    , size          :: MantineNumberSizeImpl
    , spacing       :: MantineNumberSizeImpl
    , type          :: ListTypeImpl
    , withPadding   :: Boolean
    )

listItem
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_ListItem
  => Union attrsImpl attrsImpl_ Props_ListItemImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
listItem = element (unsafeCoerce listItemComponent) <<< toNative

listItem_ :: Array JSX -> JSX
listItem_ children = listItem { children }

foreign import listItemComponent :: ReactComponent (Record Props_ListItemImpl)

type Props_ListItem =
  Props_Common
    ( children :: Array JSX
    , icon     :: JSX
    )

type Props_ListItemImpl =
  Props_CommonImpl
    ( children :: Array JSX
    , icon     :: JSX
    )
