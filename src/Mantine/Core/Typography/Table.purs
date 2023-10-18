module Mantine.Core.Typography.Table
  ( table
  , table_
  , TableProps
  , TableCaptionSide(..)
  ) where

import Data.Either (Either(..))
import Mantine.Core.Prelude

table :: (TableProps -> TableProps) -> JSX
table = mkComponent tableComponent toNative defaultTableProps

table_ :: Array JSX -> JSX
table_ children = table _ { children = children }

foreign import tableComponent :: ReactComponent TablePropsImpl

type TableProps =
  ThemingProps
    ( children          :: Array JSX
    , captionSide       :: TableCaptionSide
    , fontSize          :: MantineNumberSize
    , highlightOnHover  :: Boolean
    , horizontalSpacing :: MantineNumberSize
    , striped           :: Boolean
    , verticalSpacing   :: MantineNumberSize
    , withBorder        :: Boolean
    , withColumnBorders :: Boolean
    )

defaultTableProps :: TableProps
defaultTableProps =
  defaultThemingProps
    { horizontalSpacing: Right ExtraSmall
    , fontSize:          Right Small
    , verticalSpacing:   Left 7.0
    } `union` defaultValue

data TableCaptionSide
  = TableCaptionSideBottom
  | TableCaptionSideTop

instance DefaultValue TableCaptionSide where defaultValue = TableCaptionSideTop

instance ToFFI TableCaptionSide String where
  toNative = case _ of
    TableCaptionSideBottom -> "bottom"
    TableCaptionSideTop    -> "top"

type TablePropsImpl =
  ThemingPropsImpl
    ( children          :: Array JSX
    , captionSide       :: String
    , fontSize          :: MantineNumberSizeImpl
    , highlightOnHover  :: Boolean
    , horizontalSpacing :: MantineNumberSizeImpl
    , striped           :: Boolean
    , verticalSpacing   :: MantineNumberSizeImpl
    , withBorder        :: Boolean
    , withColumnBorders :: Boolean
    )
