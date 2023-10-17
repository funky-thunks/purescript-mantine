module Mantine.Core.Layout.Grid
  ( grid
  , grid_
  , GridProps

  , gridCol
  , gridCol_
  , GridColProps
  , GridColSpan(..)

  , module Mantine.Core.Common
  ) where

import Prelude
import Data.Default (defaultValue)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Mantine.Core.Common (AlignContent(..), JustifyContent(..), MantineNumberSize, MantineSize(..), Pixels)
import Mantine.Core.Common as MC
import Mantine.FFI (class ToFFI, toNative)
import React.Basic (ReactComponent, element)
import React.Basic.Hooks (JSX)
import Record (rename, union)
import Type.Proxy (Proxy(..))
import Untagged.Union (type (|+|), asOneOf)

grid :: (GridProps -> GridProps) -> JSX
grid setProps = element gridComponent (toNative (setProps defaultGridProps))

grid_ :: Array JSX -> JSX
grid_ children = grid _ { children = children }

foreign import gridComponent :: ReactComponent GridPropsImpl

type GridProps =
  MC.ThemingProps
    ( align    :: AlignContent
    , children :: Array JSX
    , columns  :: Int
    , grow     :: Boolean
    , gutter   :: MantineNumberSize
    , justify  :: JustifyContent
    )

defaultGridProps :: GridProps
defaultGridProps =
  MC.defaultThemingProps
    { align:    AlignContentStretch
    , columns:  12
    , gutter:   pure Medium
    , justify:  JustifyContentFlexStart
    } `union` defaultValue

type GridPropsImpl =
  MC.ThemingPropsImpl
    ( align    :: String
    , children :: Array JSX
    , columns  :: Number
    , grow     :: Boolean
    , gutter   :: MC.MantineNumberSizeImpl
    , justify  :: String
    )

gridCol :: (GridColProps -> GridColProps) -> JSX
gridCol setProps = element gridColComponent (gridColToImpl (setProps defaultGridColProps))

gridCol_ :: Array JSX -> JSX
gridCol_ children = gridCol _ { children = children }

foreign import gridColComponent :: ReactComponent GridColPropsImpl

type GridColProps =
  MC.ThemingProps
    ( children  :: Array JSX

    , span      :: Maybe GridColSpan
    , spanXs    :: Maybe GridColSpan
    , spanSm    :: Maybe GridColSpan
    , spanMd    :: Maybe GridColSpan
    , spanLg    :: Maybe GridColSpan
    , spanXl    :: Maybe GridColSpan

    , order     :: Maybe Int
    , orderXs   :: Maybe Int
    , orderSm   :: Maybe Int
    , orderMd   :: Maybe Int
    , orderLg   :: Maybe Int
    , orderXl   :: Maybe Int

    , offset    :: Maybe Pixels
    , offsetXs  :: Maybe Pixels
    , offsetSm  :: Maybe Pixels
    , offsetMd  :: Maybe Pixels
    , offsetLg  :: Maybe Pixels
    , offsetXl  :: Maybe Pixels
    )

data GridColSpan
  = ColSpanNumber Int
  | ColSpanAuto
  | ColSpanContent

type ColSpanImpl = Number |+| String

instance ToFFI GridColSpan ColSpanImpl where
  toNative = case _ of
    ColSpanNumber n -> asOneOf (toNumber n)
    ColSpanAuto     -> asOneOf "auto"
    ColSpanContent  -> asOneOf "content"

defaultGridColProps :: GridColProps
defaultGridColProps =
  MC.defaultThemingProps
    { children: []

    , span:     Nothing
    , spanXs:   Nothing
    , spanSm:   Nothing
    , spanMd:   Nothing
    , spanLg:   Nothing
    , spanXl:   Nothing

    , order:    Nothing
    , orderXs:  Nothing
    , orderSm:  Nothing
    , orderMd:  Nothing
    , orderLg:  Nothing
    , orderXl:  Nothing

    , offset:   Nothing
    , offsetXs: Nothing
    , offsetSm: Nothing
    , offsetMd: Nothing
    , offsetLg: Nothing
    , offsetXl: Nothing
    }

type GridColPropsImpl =
  MC.ThemingPropsImpl
    ( children :: Array JSX

    , span     :: Nullable ColSpanImpl
    , xs       :: Nullable ColSpanImpl
    , sm       :: Nullable ColSpanImpl
    , md       :: Nullable ColSpanImpl
    , lg       :: Nullable ColSpanImpl
    , xl       :: Nullable ColSpanImpl

    , order    :: Nullable Number
    , orderXs  :: Nullable Number
    , orderSm  :: Nullable Number
    , orderMd  :: Nullable Number
    , orderLg  :: Nullable Number
    , orderXl  :: Nullable Number

    , offset   :: Nullable Pixels
    , offsetXs :: Nullable Pixels
    , offsetSm :: Nullable Pixels
    , offsetMd :: Nullable Pixels
    , offsetLg :: Nullable Pixels
    , offsetXl :: Nullable Pixels
    )

gridColToImpl :: GridColProps -> GridColPropsImpl
gridColToImpl = toNative
            <<< rename (Proxy :: Proxy "spanXs") (Proxy :: Proxy "xs")
            <<< rename (Proxy :: Proxy "spanSm") (Proxy :: Proxy "sm")
            <<< rename (Proxy :: Proxy "spanMd") (Proxy :: Proxy "md")
            <<< rename (Proxy :: Proxy "spanLg") (Proxy :: Proxy "lg")
            <<< rename (Proxy :: Proxy "spanXl") (Proxy :: Proxy "xl")
