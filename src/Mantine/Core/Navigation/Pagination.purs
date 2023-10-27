module Mantine.Core.Navigation.Pagination
  ( pagination
  , pagination_
  , Page(..)
  , PageCount(..)
  , PaginationProps
  ) where

import Prelude
import Data.Int (floor, toNumber)
import Data.Newtype (class Newtype, unwrap, wrap)
import Mantine.Core.Prelude

pagination :: (PaginationProps -> PaginationProps) -> JSX
pagination = mkComponentWithDefault paginationComponent defaultPaginationProps

pagination_ :: JSX
pagination_ = pagination identity

type PaginationProps =
  ThemingProps
    ( align        :: Maybe AlignItems
    , boundaries   :: PageCount
    , color        :: Maybe MantineColor
    , disabled     :: Boolean
    , grow         :: Boolean
    , initialPage  :: Maybe Page
    , noWrap       :: Boolean
    , onChange     :: ValueHandler Page
    , page         :: Maybe Page
    , position     :: Maybe Position
    , radius       :: MantineNumberSize
    , siblings     :: PageCount
    , size         :: MantineNumberSize
    , spacing      :: Maybe MantineNumberSize
    , total        :: PageCount
    , withControls :: Boolean
    , withEdges    :: Boolean
    )

defaultPaginationProps :: PaginationProps
defaultPaginationProps =
  defaultThemingProps
    { boundaries:   PageCount 1
    , radius:       Preset Small
    , siblings:     PageCount 1
    , size:         Preset Medium
    , total:        PageCount 1
    , withControls: true
    }

newtype Page = Page Int

derive instance ntPage :: Newtype Page _
derive newtype instance eqPage   :: Eq   Page
derive newtype instance ordPage  :: Ord  Page
derive newtype instance showPage :: Show Page

instance ToFFI Page Number where
  toNative = toNumber <<< unwrap

instance FromFFI Number Page where
  fromNative = wrap <<< floor

newtype PageCount = PageCount Int

derive instance ntPageCount :: Newtype PageCount _
derive newtype instance eqPageCount   :: Eq   PageCount
derive newtype instance ordPageCount  :: Ord  PageCount
derive newtype instance showPageCount :: Show PageCount

instance ToFFI PageCount Number where
  toNative = toNumber <<< unwrap

type PaginationPropsImpl =
  ThemingPropsImpl
    ( align        :: Nullable String
    , boundaries   :: Number
    , color        :: Nullable String
    , disabled     :: Boolean
    , grow         :: Boolean
    , initialPage  :: Nullable Number
    , noWrap       :: Boolean
    , onChange     :: EffectFn1 Number Unit
    , page         :: Nullable Number
    , position     :: Nullable String
    , radius       :: MantineNumberSizeImpl
    , siblings     :: Number
    , size         :: MantineNumberSizeImpl
    , spacing      :: Nullable MantineNumberSizeImpl
    , total        :: Number
    , withControls :: Boolean
    , withEdges    :: Boolean
    )

foreign import paginationComponent :: ReactComponent PaginationPropsImpl
