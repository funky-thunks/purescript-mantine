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
--   , getItemProps    :: (page: number) => Record<string, any>
--   , lastIcon        :: PaginationIcon
--   , nextIcon        :: PaginationIcon
--   , previousIcon    :: PaginationIcon
--   }

type PaginationProps =
  MantineComponent
    ( boundaries     :: PageCount
    , color          :: Maybe MantineColor
    , disabled       :: Boolean
    , gap            :: Maybe MantineNumberSize
    , onFirstPage    :: Effect Unit
    , onLastPage     :: Effect Unit
    , onNextPage     :: Effect Unit
    , onPreviousPage :: Effect Unit
    , radius         :: MantineNumberSize
    , siblings       :: PageCount
    , size           :: MantineNumberSize
    , total          :: PageCount
    , withControls   :: Boolean
    , withEdges      :: Boolean
    | Controlled Page
    )

defaultPaginationProps :: PaginationProps
defaultPaginationProps =
  defaultMantineComponent
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

type PageImpl = Number

instance ToFFI Page PageImpl where
  toNative = toNumber <<< unwrap

instance FromFFI PageImpl Page where
  fromNative = wrap <<< floor

newtype PageCount = PageCount Int

derive instance ntPageCount :: Newtype PageCount _
derive newtype instance eqPageCount   :: Eq   PageCount
derive newtype instance ordPageCount  :: Ord  PageCount
derive newtype instance showPageCount :: Show PageCount

type PageCountImpl = Number

instance ToFFI PageCount PageCountImpl where
  toNative = toNumber <<< unwrap

type PaginationPropsImpl =
  MantineComponentImpl
    ( boundaries     :: PageCountImpl
    , color          :: Nullable MantineColorImpl
    , disabled       :: Boolean
    , gap            :: Nullable MantineNumberSizeImpl
    , onFirstPage    :: Effect Unit
    , onLastPage     :: Effect Unit
    , onNextPage     :: Effect Unit
    , onPreviousPage :: Effect Unit
    , radius         :: MantineNumberSizeImpl
    , siblings       :: PageCountImpl
    , size           :: MantineNumberSizeImpl
    , total          :: PageCountImpl
    , withControls   :: Boolean
    , withEdges      :: Boolean
    | ControlledImpl PageImpl
    )
