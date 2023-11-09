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
  MantineComponent
    ( align    :: AlignItems
    , children :: Array JSX
    , columns  :: Int
    , grow     :: Boolean
    , gutter   :: Maybe (FixedOrResponsive MantineSpacing)
    , justify  :: JustifyContent
    )

defaultGridProps :: GridProps
defaultGridProps =
  defaultMantineComponent
    { align:   AlignItemsStretch
    , columns: 12
    , justify: JustifyContentFlexStart
    }

type GridPropsImpl =
  MantineComponentImpl
    ( align    :: AlignItemsImpl
    , children :: Array JSX
    , columns  :: Number
    , grow     :: Boolean
    , gutter   :: Nullable (FixedOrResponsiveImpl MantineSpacingImpl)
    , justify  :: JustifyContentImpl
    )

gridCol :: (GridColProps -> GridColProps) -> JSX
gridCol = mkTrivialComponent gridColComponent

gridCol_ :: Array JSX -> JSX
gridCol_ children = gridCol _ { children = children }

foreign import gridColComponent :: ReactComponent GridColPropsImpl

type GridColProps =
  MantineComponent
    ( children :: Array JSX
    , offset   :: Maybe (FixedOrResponsive Pixels)
    , order    :: Maybe (FixedOrResponsive Int)
    , span     :: Maybe (FixedOrResponsive GridColSpan)
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
  MantineComponentImpl
    ( children :: Array JSX
    , offset   :: Nullable (FixedOrResponsiveImpl Pixels)
    , order    :: Nullable (FixedOrResponsiveImpl Number)
    , span     :: Nullable (FixedOrResponsiveImpl ColSpanImpl)
    )
