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
    , gutter   :: Maybe (FixedOrResponsive MantineNumberSize)
    , justify  :: JustifyContent
    )

defaultGridProps :: GridProps
defaultGridProps =
  defaultThemingProps
    { align:   AlignContentStretch
    , columns: 12
    , justify: JustifyContentFlexStart
    }

type GridPropsImpl =
  ThemingPropsImpl
    ( align    :: String
    , children :: Array JSX
    , columns  :: Number
    , grow     :: Boolean
    , gutter   :: Nullable (FixedOrResponsiveImpl MantineNumberSizeImpl)
    , justify  :: String
    )

gridCol :: (GridColProps -> GridColProps) -> JSX
gridCol = mkTrivialComponent gridColComponent

gridCol_ :: Array JSX -> JSX
gridCol_ children = gridCol _ { children = children }

foreign import gridColComponent :: ReactComponent GridColPropsImpl

type GridColProps =
  ThemingProps
    ( children :: Array JSX
    , span     :: Maybe (FixedOrResponsive GridColSpan)
    , order    :: Maybe (FixedOrResponsive Int)
    , offset   :: Maybe (FixedOrResponsive Pixels)
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
    , span     :: Nullable (FixedOrResponsiveImpl ColSpanImpl)
    , order    :: Nullable (FixedOrResponsiveImpl Number)
    , offset   :: Nullable (FixedOrResponsiveImpl Pixels)
    )
