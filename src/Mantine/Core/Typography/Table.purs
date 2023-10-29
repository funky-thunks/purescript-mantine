module Mantine.Core.Typography.Table
  ( table
  , table_
  , TableProps
  , TableCaptionSide(..)
  ) where

import Mantine.Core.Prelude

table :: (TableProps -> TableProps) -> JSX
table = mkComponent tableComponent toNative defaultTableProps

table_ :: Array JSX -> JSX
table_ children = table _ { children = children }

foreign import tableComponent :: ReactComponent TablePropsImpl

type TableProps =
  ThemingProps
    ( captionSide       :: TableCaptionSide
    , children          :: Array JSX
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
    { horizontalSpacing: Preset ExtraSmall
    , fontSize:          Preset Small
    , verticalSpacing:   Custom 7.0
    }

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
    ( captionSide       :: String
    , children          :: Array JSX
    , fontSize          :: MantineNumberSizeImpl
    , highlightOnHover  :: Boolean
    , horizontalSpacing :: MantineNumberSizeImpl
    , striped           :: Boolean
    , verticalSpacing   :: MantineNumberSizeImpl
    , withBorder        :: Boolean
    , withColumnBorders :: Boolean
    )
