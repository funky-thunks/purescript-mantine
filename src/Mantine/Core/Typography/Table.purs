module Mantine.Core.Typography.Table
  ( table
  , table_
  , Props_Table
  , Props_TableImpl
  , TableCaptionSide(..)
  , TableCaptionSideImpl

  , tableCaption
  , tableCaption_
  , Props_TableCaption
  , Props_TableCaptionImpl

  , tableScrollContainer
  , tableScrollContainer_
  , Props_TableScrollContainer
  , Props_TableScrollContainerImpl

  , tableTbody
  , tableTbody_
  , Props_TableTbody
  , Props_TableTbodyImpl

  , tableTd
  , tableTd_
  , Props_TableTd
  , Props_TableTdImpl

  , tableTfoot
  , tableTfoot_
  , Props_TableTfoot
  , Props_TableTfootImpl

  , tableTh
  , tableTh_
  , Props_TableTh
  , Props_TableThImpl

  , tableThead
  , tableThead_
  , Props_TableThead
  , Props_TableTheadImpl

  , tableTr
  , tableTr_
  , Props_TableTr
  , Props_TableTrImpl

  , WithChildren
  , WithChildrenImpl
  ) where

import Mantine.Core.Prelude

table
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Table
  => Union attrsImpl attrsImpl_ Props_TableImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
table = element (unsafeCoerce tableComponent) <<< toNative

table_ :: Array JSX -> JSX
table_ children = table { children }

foreign import tableComponent :: ReactComponent (Record Props_TableImpl)

-- Not supported properties
--   { data :: TableData
--   }

type Props_Table =
  WithChildren
    ( borderColor           :: MantineColor
    , captionSide           :: TableCaptionSide
    , highlightOnHover      :: Boolean
    , highlightOnHoverColor :: MantineColor
    , horizontalSpacing     :: MantineNumberSize
    , layout                :: TableLayout
    , stickyHeader          :: Boolean
    , stickyHeaderOffset    :: MantineNumberSize
    , striped               :: Boolean
    , stripedColor          :: MantineColor
    , verticalSpacing       :: MantineNumberSize
    , withColumnBorders     :: Boolean
    , withRowBorders        :: Boolean
    , withTableBorder       :: Boolean
    )

data TableCaptionSide
  = TableCaptionSideBottom
  | TableCaptionSideTop

type TableCaptionSideImpl = String

instance ToFFI TableCaptionSide TableCaptionSideImpl where
  toNative = case _ of
    TableCaptionSideBottom -> "bottom"
    TableCaptionSideTop    -> "top"

type Props_TableImpl =
  WithChildrenImpl
    ( borderColor           :: MantineColorImpl
    , captionSide           :: TableCaptionSideImpl
    , highlightOnHover      :: Boolean
    , highlightOnHoverColor :: MantineColorImpl
    , horizontalSpacing     :: MantineNumberSizeImpl
    , layout                :: TableLayoutImpl
    , stickyHeader          :: Boolean
    , stickyHeaderOffset    :: MantineNumberSizeImpl
    , striped               :: Boolean
    , stripedColor          :: MantineColorImpl
    , verticalSpacing       :: MantineNumberSizeImpl
    , withColumnBorders     :: Boolean
    , withRowBorders        :: Boolean
    , withTableBorder       :: Boolean
    )

tableCaption
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_TableCaption
  => Union attrsImpl attrsImpl_ Props_TableCaptionImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
tableCaption = element (unsafeCoerce tableCaptionComponent) <<< toNative

tableCaption_ :: Array JSX -> JSX
tableCaption_ children = tableCaption { children }

foreign import tableCaptionComponent :: ReactComponent (Record Props_TableCaptionImpl)

type Props_TableCaption     = WithChildren     ()
type Props_TableCaptionImpl = WithChildrenImpl ()

tableScrollContainer
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_TableScrollContainer
  => Union attrsImpl attrsImpl_ Props_TableScrollContainerImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
tableScrollContainer = element (unsafeCoerce tableScrollContainerComponent) <<< toNative

tableScrollContainer_ :: Array JSX -> JSX
tableScrollContainer_ children = tableScrollContainer { children }

foreign import tableScrollContainerComponent :: ReactComponent (Record Props_TableScrollContainerImpl)

type Props_TableScrollContainer     = WithChildren     ()
type Props_TableScrollContainerImpl = WithChildrenImpl ()

tableTbody
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_TableTbody
  => Union attrsImpl attrsImpl_ Props_TableTbodyImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
tableTbody = element (unsafeCoerce tableTbodyComponent) <<< toNative

tableTbody_ :: Array JSX -> JSX
tableTbody_ children = tableTbody { children }

foreign import tableTbodyComponent :: ReactComponent (Record Props_TableTbodyImpl)

type Props_TableTbody     = WithChildren     ()
type Props_TableTbodyImpl = WithChildrenImpl ()

tableTd
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_TableTd
  => Union attrsImpl attrsImpl_ Props_TableTdImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
tableTd = element (unsafeCoerce tableTdComponent) <<< toNative

tableTd_ :: Array JSX -> JSX
tableTd_ children = tableTd { children }

foreign import tableTdComponent :: ReactComponent (Record Props_TableTdImpl)

type Props_TableTd     = WithChildren     ()
type Props_TableTdImpl = WithChildrenImpl ()

tableTfoot
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_TableTfoot
  => Union attrsImpl attrsImpl_ Props_TableTfootImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
tableTfoot = element (unsafeCoerce tableTfootComponent) <<< toNative

tableTfoot_ :: Array JSX -> JSX
tableTfoot_ children = tableTfoot { children }

foreign import tableTfootComponent :: ReactComponent (Record Props_TableTfootImpl)

type Props_TableTfoot     = WithChildren     ()
type Props_TableTfootImpl = WithChildrenImpl ()

tableTh
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_TableTh
  => Union attrsImpl attrsImpl_ Props_TableThImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
tableTh = element (unsafeCoerce tableThComponent) <<< toNative

tableTh_ :: Array JSX -> JSX
tableTh_ children = tableTh { children }

foreign import tableThComponent :: ReactComponent (Record Props_TableThImpl)

type Props_TableTh     = WithChildren     ()
type Props_TableThImpl = WithChildrenImpl ()

tableThead
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_TableThead
  => Union attrsImpl attrsImpl_ Props_TableTheadImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
tableThead = element (unsafeCoerce tableTheadComponent) <<< toNative

tableThead_ :: Array JSX -> JSX
tableThead_ children = tableThead { children }

foreign import tableTheadComponent :: ReactComponent (Record Props_TableTheadImpl)

type Props_TableThead     = WithChildren     ()
type Props_TableTheadImpl = WithChildrenImpl ()

tableTr
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_TableTr
  => Union attrsImpl attrsImpl_ Props_TableTrImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
tableTr = element (unsafeCoerce tableTrComponent) <<< toNative

tableTr_ :: String -> Array JSX -> JSX
tableTr_ key children = tableTr { key, children }

foreign import tableTrComponent :: ReactComponent (Record Props_TableTrImpl)

type Props_TableTr     = WithChildren     ()
type Props_TableTrImpl = WithChildrenImpl ()

type WithChildren     rest = Props_Common     ( children :: Array JSX | rest )
type WithChildrenImpl rest = Props_CommonImpl ( children :: Array JSX | rest )
