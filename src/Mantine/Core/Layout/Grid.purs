module Mantine.Core.Layout.Grid
  ( grid
  , grid_
  , Props_Grid
  , Props_GridImpl

  , gridCol
  , gridCol_
  , Props_GridCol
  , Props_GridColImpl
  , GridColSpan(..)
  , GridColSpanImpl

  , gridSpan
  , gridOffset
  ) where

import Data.Int (toNumber)
import Mantine.Core.Prelude

grid
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Grid
  => Union attrsImpl attrsImpl_ Props_GridImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
grid = element (unsafeCoerce gridComponent) <<< toNative

grid_ :: Array JSX -> JSX
grid_ children = grid { children }

foreign import gridComponent :: ReactComponent (Record Props_GridImpl)

type Props_Grid =
  Props_Common
    ( align    :: AlignItems
    , children :: Array JSX
    , columns  :: Int
    , grow     :: Boolean
    , gutter   :: FixedOrResponsive MantineSpacing
    , justify  :: JustifyContent
    , overflow :: Overflow
    )

type Props_GridImpl =
  Props_CommonImpl
    ( align    :: AlignItemsImpl
    , children :: Array JSX
    , columns  :: Number
    , grow     :: Boolean
    , gutter   :: FixedOrResponsiveImpl MantineSpacingImpl
    , justify  :: JustifyContentImpl
    , overflow :: OverflowImpl
    )

gridCol
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_GridCol
  => Union attrsImpl attrsImpl_ Props_GridColImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
gridCol = element (unsafeCoerce gridColComponent) <<< toNative

gridCol_ :: Array JSX -> JSX
gridCol_ children = gridCol { children }

foreign import gridColComponent :: ReactComponent (Record Props_GridColImpl)

type Props_GridCol =
  Props_Common
    ( children :: Array JSX
    , offset   :: FixedOrResponsive Int
    , order    :: FixedOrResponsive Int
    , span     :: FixedOrResponsive GridColSpan
    )

data GridColSpan
  = GridColSpanNumber Int
  | GridColSpanAuto
  | GridColSpanContent

type GridColSpanImpl = Number |+| String

instance ToFFI GridColSpan GridColSpanImpl where
  toNative = case _ of
    GridColSpanNumber n -> asOneOf (toNumber n)
    GridColSpanAuto     -> asOneOf "auto"
    GridColSpanContent  -> asOneOf "content"

type Props_GridColImpl =
  Props_CommonImpl
    ( children :: Array JSX
    , offset   :: FixedOrResponsiveImpl Number
    , order    :: FixedOrResponsiveImpl Number
    , span     :: FixedOrResponsiveImpl GridColSpanImpl
    )

gridSpan :: Int -> FixedOrResponsive GridColSpan
gridSpan = Fixed <<< GridColSpanNumber

gridOffset :: Int -> FixedOrResponsive Int
gridOffset = Fixed
