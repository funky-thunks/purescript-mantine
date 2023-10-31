module Mantine.Core.Navigation.Pagination
  ( pagination
  , pagination_
  , Page(..)
  , PageCount(..)
  , PaginationProps
  ) where

import Prelude (class Eq, class Ord, class Show)
import Data.Int (floor, toNumber)
import Data.Newtype (class Newtype, unwrap, wrap)
import Mantine.Core.Prelude

-- Not supported components: Pagination.Root and its children: Pagination.First, Pagination.Previous, Pagination.Items, Pagination.Next, Pagination.Last

pagination :: (PaginationProps -> PaginationProps) -> JSX
pagination = mkComponentWithDefault paginationComponent defaultPaginationProps

pagination_ :: JSX
pagination_ = pagination identity

foreign import paginationComponent :: ReactComponent PaginationPropsImpl

-- Not supported properties
--   { dotsIcon        :: PaginationIcon
--   , firstIcon       :: PaginationIcon
--   , getControlProps :: (control: "first" | "last" | "next" | "previous") => Record<string, any>
--   , getItemProp     :: (page: number) => Record<string, any>
--   , lastIcon        :: PaginationIcon
--   , nextIcon        :: PaginationIcon
--   , previousIcon    :: PaginationIcon
--   }

type PaginationProps =
  ThemingProps
    ( align          :: Maybe AlignItems
    , boundaries     :: PageCount
    , color          :: Maybe MantineColor
    , defaultValue   :: Maybe Page
    , disabled       :: Boolean
    , grow           :: Boolean
    , noWrap         :: Boolean
    , onChange       :: ValueHandler Page
    , onFirstPage    :: Effect Unit
    , onLastPage     :: Effect Unit
    , onNextPage     :: Effect Unit
    , onPreviousPage :: Effect Unit
    , position       :: Maybe Position
    , radius         :: MantineNumberSize
    , siblings       :: PageCount
    , size           :: MantineNumberSize
    , spacing        :: Maybe MantineNumberSize
    , total          :: PageCount
    , value          :: Maybe Page
    , withControls   :: Boolean
    , withEdges      :: Boolean
    )

defaultPaginationProps :: PaginationProps
defaultPaginationProps =
  defaultThemingProps
    { boundaries:     PageCount 1
    , onFirstPage:    pure unit
    , onLastPage:     pure unit
    , onNextPage:     pure unit
    , onPreviousPage: pure unit
    , radius:         Preset Small
    , siblings:       PageCount 1
    , size:           Preset Medium
    , total:          PageCount 1
    , withControls:   true
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
    ( align          :: Nullable String
    , boundaries     :: Number
    , color          :: Nullable String
    , defaultValue   :: Nullable Number
    , disabled       :: Boolean
    , grow           :: Boolean
    , noWrap         :: Boolean
    , onChange       :: EffectFn1 Number Unit
    , position       :: Nullable String
    , radius         :: MantineNumberSizeImpl
    , siblings       :: Number
    , size           :: MantineNumberSizeImpl
    , spacing        :: Nullable MantineNumberSizeImpl
    , total          :: Number
    , value          :: Nullable Number
    , withControls   :: Boolean
    , withEdges      :: Boolean
    , onFirstPage    :: Effect Unit
    , onLastPage     :: Effect Unit
    , onNextPage     :: Effect Unit
    , onPreviousPage :: Effect Unit
    )
