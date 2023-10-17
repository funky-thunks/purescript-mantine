module Mantine.Core.Navigation.Pagination
  ( pagination
  , pagination_
  , Page(..)
  , PageCount(..)
  , PaginationProps

  , module Mantine.Core.Common
  ) where

import Prelude
import Data.Default (defaultValue)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (Nullable)
import Effect.Uncurried (EffectFn1)
import Mantine.Core.Common (AlignItems(..), MantineColor(..), MantineNumberSize, MantineSize(..), Position(..), ValueHandler)
import Mantine.Core.Common as MC
import Mantine.FFI (class FromFFI, class ToFFI, toNative)
import React.Basic (ReactComponent, element)
import React.Basic.Hooks (JSX)
import Record (union)

pagination :: (PaginationProps -> PaginationProps) -> JSX
pagination setProps = element paginationComponent (toNative (setProps defaultPaginationProps))

pagination_ :: JSX
pagination_ = pagination identity

type PaginationProps =
  MC.ThemingProps
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
  MC.defaultThemingProps
    { boundaries:   PageCount 1
    , radius:       pure MC.Small
    , siblings:     PageCount 1
    , size:         pure MC.Medium
    , total:        PageCount 1
    , withControls: true
    } `union` defaultValue

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
  MC.ThemingPropsImpl
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
    , radius       :: MC.MantineNumberSizeImpl
    , siblings     :: Number
    , size         :: MC.MantineNumberSizeImpl
    , spacing      :: Nullable MC.MantineNumberSizeImpl
    , total        :: Number
    , withControls :: Boolean
    , withEdges    :: Boolean
    )

foreign import paginationComponent :: ReactComponent PaginationPropsImpl
