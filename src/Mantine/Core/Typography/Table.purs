module Mantine.Core.Typography.Table
  ( table
  , table_
  , TableProps
  , TableCaptionSide(..)

  , tableCaption
  , tableCaption_
  , TableCaptionProps

  , tableScrollContainer
  , tableScrollContainer_
  , TableScrollContainerProps

  , tableTbody
  , tableTbody_
  , TableTbodyProps

  , tableTd
  , tableTd_
  , TableTdProps

  , tableTfoot
  , tableTfoot_
  , TableTfootProps

  , tableTh
  , tableTh_
  , TableThProps

  , tableThead
  , tableThead_
  , TableTheadProps

  , tableTr
  , tableTr_
  , TableTrProps

  , WithChildren
  ) where

import Mantine.Core.Prelude

table :: (TableProps -> TableProps) -> JSX
table = mkComponentWithDefault tableComponent defaultTableProps

table_ :: Array JSX -> JSX
table_ children = table _ { children = children }

foreign import tableComponent :: ReactComponent TablePropsImpl

type TableProps =
  WithChildren
    ( borderColor           :: Maybe MantineColor
    , captionSide           :: TableCaptionSide
    , highlightOnHover      :: Boolean
    , highlightOnHoverColor :: Maybe MantineColor
    , horizontalSpacing     :: MantineNumberSize
    , layout                :: TableLayout
    , striped               :: Boolean
    , stripedColor          :: Maybe MantineColor
    , verticalSpacing       :: MantineNumberSize
    , withColumnBorders     :: Boolean
    , withRowBorders        :: Boolean
    , withTableBorder       :: Boolean
    )

defaultTableProps :: TableProps
defaultTableProps =
  defaultThemingProps
    { horizontalSpacing: Preset ExtraSmall
    , verticalSpacing:   Custom 7.0
    , withRowBorders:    true
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
  WithChildrenImpl
    ( borderColor           :: Nullable String
    , captionSide           :: String
    , highlightOnHover      :: Boolean
    , highlightOnHoverColor :: Nullable String
    , horizontalSpacing     :: MantineNumberSizeImpl
    , layout                :: String
    , striped               :: Boolean
    , stripedColor          :: Nullable String
    , verticalSpacing       :: MantineNumberSizeImpl
    , withColumnBorders     :: Boolean
    , withRowBorders        :: Boolean
    , withTableBorder       :: Boolean
    )

tableCaption :: (TableCaptionProps -> TableCaptionProps) -> JSX
tableCaption = mkTrivialComponent tableCaptionComponent

tableCaption_ :: Array JSX -> JSX
tableCaption_ children = tableCaption _ { children = children }

foreign import tableCaptionComponent :: ReactComponent TableCaptionPropsImpl

type TableCaptionProps     = WithChildren     ()
type TableCaptionPropsImpl = WithChildrenImpl ()

tableScrollContainer :: (TableScrollContainerProps -> TableScrollContainerProps) -> JSX
tableScrollContainer = mkTrivialComponent tableScrollContainerComponent

tableScrollContainer_ :: Array JSX -> JSX
tableScrollContainer_ children = tableScrollContainer _ { children = children }

foreign import tableScrollContainerComponent :: ReactComponent TableScrollContainerPropsImpl

type TableScrollContainerProps     = WithChildren     ()
type TableScrollContainerPropsImpl = WithChildrenImpl ()

tableTbody :: (TableTbodyProps -> TableTbodyProps) -> JSX
tableTbody = mkTrivialComponent tableTbodyComponent

tableTbody_ :: Array JSX -> JSX
tableTbody_ children = tableTbody _ { children = children }

foreign import tableTbodyComponent :: ReactComponent TableTbodyPropsImpl

type TableTbodyProps     = WithChildren     ()
type TableTbodyPropsImpl = WithChildrenImpl ()

tableTd :: (TableTdProps -> TableTdProps) -> JSX
tableTd = mkTrivialComponent tableTdComponent

tableTd_ :: Array JSX -> JSX
tableTd_ children = tableTd _ { children = children }

foreign import tableTdComponent :: ReactComponent TableTdPropsImpl

type TableTdProps     = WithChildren     ()
type TableTdPropsImpl = WithChildrenImpl ()

tableTfoot :: (TableTfootProps -> TableTfootProps) -> JSX
tableTfoot = mkTrivialComponent tableTfootComponent

tableTfoot_ :: Array JSX -> JSX
tableTfoot_ children = tableTfoot _ { children = children }

foreign import tableTfootComponent :: ReactComponent TableTfootPropsImpl

type TableTfootProps     = WithChildren     ()
type TableTfootPropsImpl = WithChildrenImpl ()

tableTh :: (TableThProps -> TableThProps) -> JSX
tableTh = mkTrivialComponent tableThComponent

tableTh_ :: Array JSX -> JSX
tableTh_ children = tableTh _ { children = children }

foreign import tableThComponent :: ReactComponent TableThPropsImpl

type TableThProps     = WithChildren     ()
type TableThPropsImpl = WithChildrenImpl ()

tableThead :: (TableTheadProps -> TableTheadProps) -> JSX
tableThead = mkTrivialComponent tableTheadComponent

tableThead_ :: Array JSX -> JSX
tableThead_ children = tableThead _ { children = children }

foreign import tableTheadComponent :: ReactComponent TableTheadPropsImpl

type TableTheadProps     = WithChildren     ()
type TableTheadPropsImpl = WithChildrenImpl ()

tableTr :: String -> (TableTrProps -> TableTrProps) -> JSX
tableTr = mkComponentWithDefault tableTrComponent <<< defaultTableTr

tableTr_ :: String -> Array JSX -> JSX
tableTr_ key children = (tableTr key) _ { children = children }

foreign import tableTrComponent :: ReactComponent TableTrPropsImpl

type TableTrProps     = WithChildren     ( key :: String )
type TableTrPropsImpl = WithChildrenImpl ( key :: String )

defaultTableTr :: String -> TableTrProps
defaultTableTr key = defaultThemingProps { key }

type WithChildren     rest = ThemingProps     ( children :: Array JSX | rest )
type WithChildrenImpl rest = ThemingPropsImpl ( children :: Array JSX | rest )
