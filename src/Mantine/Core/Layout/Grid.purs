module Mantine.Core.Layout.Grid
  ( grid
  , grid_
  , GridProps

  , gridCol
  , gridCol_
  , GridColProps
  , GridColSpan(..)
  ) where

import Data.Int (toNumber)
import Mantine.Core.Prelude

grid :: (GridProps -> GridProps) -> JSX
grid = mkComponentWithDefault gridComponent defaultGridProps

grid_ :: Array JSX -> JSX
grid_ children = grid _ { children = children }

foreign import gridComponent :: ReactComponent GridPropsImpl

type GridProps =
  ThemingProps
    ( align    :: AlignContent
    , children :: Array JSX
    , columns  :: Int
    , grow     :: Boolean
    , gutter   :: MantineNumberSize
    , justify  :: JustifyContent
    )

defaultGridProps :: GridProps
defaultGridProps =
  defaultThemingProps
    { align:    AlignContentStretch
    , columns:  12
    , gutter:   Preset Medium
    , justify:  JustifyContentFlexStart
    }

type GridPropsImpl =
  ThemingPropsImpl
    ( align    :: String
    , children :: Array JSX
    , columns  :: Number
    , grow     :: Boolean
    , gutter   :: MantineNumberSizeImpl
    , justify  :: String
    )

gridCol :: (GridColProps -> GridColProps) -> JSX
gridCol = mkComponent gridColComponent gridColToImpl defaultThemingProps_

gridCol_ :: Array JSX -> JSX
gridCol_ children = gridCol _ { children = children }

foreign import gridColComponent :: ReactComponent GridColPropsImpl

type GridColProps =
  ThemingProps
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

type GridColPropsImpl =
  ThemingPropsImpl
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
