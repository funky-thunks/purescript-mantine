module Mantine.Core.Navigation.Pagination
  ( pagination
  , pagination_
  , Props_Pagination
  , Props_PaginationImpl
  , Page(..)
  , PageImpl
  , PageCount(..)
  , PageCountImpl
  ) where

import Prelude (class Eq, class Ord, class Show)
import Data.Int (floor, toNumber)
import Data.Newtype (class Newtype, unwrap, wrap)
import Mantine.Core.Prelude

-- Not supported components: Pagination.Root and its children: Pagination.First, Pagination.Previous, Pagination.Items, Pagination.Next, Pagination.Last

pagination
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Pagination
  => Union attrsImpl attrsImpl_ Props_PaginationImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
pagination = element (unsafeCoerce paginationComponent) <<< toNative

pagination_ :: JSX
pagination_ = pagination {}

foreign import paginationComponent :: ReactComponent (Record Props_PaginationImpl)

-- Not supported properties
--   { dotsIcon        :: PaginationIcon
--   , firstIcon       :: PaginationIcon
--   , getControlProps :: (control: "first" | "last" | "next" | "previous") => Record<string, any>
--   , getItemProps    :: (page: number) => Record<string, any>
--   , lastIcon        :: PaginationIcon
--   , nextIcon        :: PaginationIcon
--   , previousIcon    :: PaginationIcon
--   }

type Props_Pagination =
  Props_Common
    ( boundaries     :: PageCount
    , color          :: MantineColor
    , disabled       :: Boolean
    , gap            :: MantineNumberSize
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
    | Controlled ValueHandler Page
    )

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

type Props_PaginationImpl =
  Props_CommonImpl
    ( boundaries     :: PageCountImpl
    , color          :: MantineColorImpl
    , disabled       :: Boolean
    , gap            :: MantineNumberSizeImpl
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
    | ControlledImpl ValueHandlerImpl PageImpl
    )
