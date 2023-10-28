module Mantine.Core.Inputs.TransferList
  ( transferList
  , Split(..)
  , splitted
  , TransferListProps
  , TransferListItemComponentProps
  , TransferListData
  , TransferListItem
  ) where

import Data.Monoid (class Monoid)
import Effect.Uncurried (EffectFn2, mkEffectFn2)
import Mantine.Core.Prelude

transferList :: TransferListData -> (TransferListProps -> TransferListProps) -> JSX
transferList = mkComponent transferListComponent transferListToImpl <<< defaultTransferListProps

foreign import transferListComponent :: ReactComponent TransferListPropsImpl

-- Not supported properties
--   { listComponent :: any
--   }

type TransferListProps =
  ThemingProps
    ( breakpoint                :: Maybe MantineNumberSize
    , filter                    :: Maybe (String -> TransferListItem -> Effect Boolean)
    , initialSelection          :: Maybe (Split (Array String))
    , itemComponent             :: Maybe (TransferListItemComponentProps     -> JSX)
    , limit                     :: Maybe Int
    , listHeight                :: Maybe Pixels
    , nothingFound              :: Maybe (Split JSX)
    , onChange                  :: ValueHandler TransferListData
    , onSearch                  :: ValueHandler (Split String)
    , placeholder               :: Maybe (Split JSX)
    , radius                    :: Maybe MantineNumberSize
    , searchPlaceholder         :: Maybe (Split String)
    , searchValues              :: Maybe (Split String)
    , showTransferAll           :: Boolean
    , titles                    :: Maybe (Split String)
    , transferAllIcon           :: Maybe ({ reversed :: Boolean } -> JSX)
    , transferAllMatchingFilter :: Boolean
    , transferIcon              :: Maybe ({ reversed :: Boolean } -> JSX)
    , value                     :: TransferListData
    )

type TransferListItemComponentProps =
  { data     :: TransferListItem
  , selected :: Boolean
  , radius   :: MantineNumberSize
  }

type TransferListData = Split (Array TransferListItem)

type TransferListItem =
  { value :: String
  , label :: String
  , group :: Maybe String
  }

defaultTransferListProps :: TransferListData -> TransferListProps
defaultTransferListProps value = defaultThemingProps { value }

type TransferListPropsImpl =
  ThemingPropsImpl
    ( breakpoint                :: Nullable MantineNumberSizeImpl
    , filter                    :: Nullable (EffectFn2 String TransferListItemImpl Boolean)
    , initialSelection          :: Nullable (SplitImpl (Array String))
    , itemComponent             :: Nullable (TransferListItemComponentPropsImpl -> JSX)
    , limit                     :: Nullable Number
    , listHeight                :: Nullable Number
    , nothingFound              :: Nullable (SplitImpl JSX)
    , onChange                  :: EffectFn1 TransferListDataImpl Unit
    , onSearch                  :: EffectFn1 (SplitImpl String) Unit
    , placeholder               :: Nullable (Array JSX)
    , radius                    :: Nullable MantineNumberSizeImpl
    , searchPlaceholder         :: Nullable (SplitImpl String)
    , searchValues              :: Nullable (SplitImpl String)
    , showTransferAll           :: Boolean
    , titles                    :: Nullable (Array String)
    , transferAllIcon           :: Nullable ({ reversed :: Boolean } -> JSX)
    , transferAllMatchingFilter :: Boolean
    , transferIcon              :: Nullable ({ reversed :: Boolean } -> JSX)
    , value                     :: TransferListDataImpl
    )

type TransferListItemComponentPropsImpl =
  { data     :: TransferListItemImpl
  , selected :: Boolean
  , radius   :: MantineNumberSizeImpl
  }

type TransferListDataImpl = SplitImpl (Array TransferListItemImpl)

type TransferListItemImpl =
  { value :: String
  , label :: String
  , group :: Nullable String
  }

transferListToImpl :: TransferListProps -> TransferListPropsImpl
transferListToImpl props =
  let rest = toNative
         <<< delete (Proxy :: Proxy "filter")
         <<< delete (Proxy :: Proxy "itemComponent")
         <<< delete (Proxy :: Proxy "transferIcon")
         <<< delete (Proxy :: Proxy "transferAllIcon")
      filter          = toNullable (filterToImpl <$> props.filter)
      itemComponent   = toNullable (map (\f -> f <<< fromNative) props.itemComponent)
      transferIcon    = toNullable props.transferIcon
      transferAllIcon = toNullable props.transferAllIcon
   in { filter, itemComponent, transferIcon, transferAllIcon } `union` rest props

filterToImpl :: (String -> TransferListItem -> Effect Boolean) -> EffectFn2 String TransferListItemImpl Boolean
filterToImpl f = mkEffectFn2 (\q i -> f q (fromNative i))

newtype Split item = Split { left :: item, right :: item }

splitted :: forall item. item -> item -> Split item
splitted left right = Split { left, right }

type SplitImpl item = Array item

instance ToFFI purs js => ToFFI (Split purs) (SplitImpl js) where
  toNative (Split { left, right }) = [ toNative left, toNative right ]

instance (FromFFI js purs, Monoid purs) => FromFFI (SplitImpl js) (Split purs) where
  fromNative = case _ of
    [left, right] -> Split (fromNative { left, right })
    _             -> Split { left: mempty, right: mempty }
