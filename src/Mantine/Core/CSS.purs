module Mantine.Core.CSS
  ( AlignContent(..)
  , AlignContentImpl
  , AlignItems(..)
  , AlignItemsImpl
  , FlexDirection(..)
  , FlexDirectionImpl
  , FlexWrap(..)
  , FlexWrapImpl
  , FontWeight(..)
  , FontWeightImpl
  , GlobalValues(..)
  , JustifyContent(..)
  , JustifyContentImpl
  , ListStyleType(..)
  , ListStyleTypeImpl
  , ObjectFit(..)
  , ObjectFitImpl
  , PointerEvents(..)
  , PointerEventsImpl
  , Position(..)
  , PositionImpl
  , TableLayout(..)
  , TableLayoutImpl
  , TextAlign(..)
  , TextAlignImpl
  ) where

import Prelude ((<>))
import Mantine.FFI (class ToFFI, toNative)

data AlignContent
  = AlignContentBaseline
  | AlignContentCenter
  | AlignContentEnd
  | AlignContentFirstBaseline
  | AlignContentFlexEnd
  | AlignContentFlexStart
  | AlignContentLastBaseline
  | AlignContentNormal
  | AlignContentSafeCenter
  | AlignContentSpaceAround
  | AlignContentSpaceBetween
  | AlignContentSpaceEvenly
  | AlignContentStart
  | AlignContentStretch
  | AlignContentUnsafeCenter
  | AlignContentGlobal GlobalValues

type AlignContentImpl = String

instance ToFFI AlignContent AlignContentImpl where
  toNative = case _ of
    AlignContentBaseline      -> "baseline"
    AlignContentCenter        -> "center"
    AlignContentEnd           -> "end"
    AlignContentFirstBaseline -> "first baseline"
    AlignContentFlexEnd       -> "flex-end"
    AlignContentFlexStart     -> "flex-start"
    AlignContentLastBaseline  -> "last baseline"
    AlignContentNormal        -> "normal"
    AlignContentSafeCenter    -> "safe center"
    AlignContentSpaceAround   -> "space-around"
    AlignContentSpaceBetween  -> "space-between"
    AlignContentSpaceEvenly   -> "space-evenly"
    AlignContentStart         -> "start"
    AlignContentStretch       -> "stretch"
    AlignContentUnsafeCenter  -> "unsafe center"
    AlignContentGlobal gv     -> toNative gv

data AlignItems
  = AlignItemsBaseline
  | AlignItemsCenter
  | AlignItemsEnd
  | AlignItemsFirstBaseline
  | AlignItemsFlexEnd
  | AlignItemsFlexStart
  | AlignItemsLastBaseline
  | AlignItemsNormal
  | AlignItemsSafeCenter
  | AlignItemsSelfEnd
  | AlignItemsSelfStart
  | AlignItemsStart
  | AlignItemsStretch
  | AlignItemsUnsafeCenter
  | AlignItemsGlobal GlobalValues

type AlignItemsImpl = String

instance ToFFI AlignItems AlignItemsImpl where
  toNative = case _ of
    AlignItemsBaseline      -> "baseline"
    AlignItemsCenter        -> "center"
    AlignItemsEnd           -> "end"
    AlignItemsFirstBaseline -> "first baseline"
    AlignItemsFlexEnd       -> "flex-end"
    AlignItemsFlexStart     -> "flex-start"
    AlignItemsLastBaseline  -> "last baseline"
    AlignItemsNormal        -> "normal"
    AlignItemsSafeCenter    -> "safe center"
    AlignItemsSelfEnd       -> "self-end"
    AlignItemsSelfStart     -> "self-start"
    AlignItemsStart         -> "start"
    AlignItemsStretch       -> "stretch"
    AlignItemsUnsafeCenter  -> "unsafe center"
    AlignItemsGlobal gv     -> toNative gv

data FlexDirection
  = FlexDirectionRow
  | FlexDirectionRowReverse
  | FlexDirectionColumn
  | FlexDirectionColumnReverse
  | FlexDirectionGlobal GlobalValues

type FlexDirectionImpl = String

instance ToFFI FlexDirection FlexDirectionImpl where
  toNative = case _ of
    FlexDirectionRow           -> "row"
    FlexDirectionRowReverse    -> "row-reverse"
    FlexDirectionColumn        -> "column"
    FlexDirectionColumnReverse -> "column-reverse"
    FlexDirectionGlobal gv     -> toNative gv

data FlexWrap
  = FlexWrapNowrap
  | FlexWrapWrap
  | FlexWrapWrapReverse
  | FlexWrapGlobal GlobalValues

type FlexWrapImpl = String

instance ToFFI FlexWrap FlexWrapImpl where
  toNative = case _ of
    FlexWrapNowrap      -> "nowrap"
    FlexWrapWrap        -> "wrap"
    FlexWrapWrapReverse -> "wrap-reverse"
    FlexWrapGlobal gv   -> toNative gv

newtype FontWeight = FontWeight Int

type FontWeightImpl = Number

instance ToFFI FontWeight FontWeightImpl where
  toNative (FontWeight fw) = toNative fw

data GlobalValues
  = Inherit
  | Initial
  | Revert
  | RevertLayer
  | Unset

instance ToFFI GlobalValues String where
  toNative = case _ of
    Inherit     -> "inherit"
    Initial     -> "initial"
    Revert      -> "revert"
    RevertLayer -> "revert-layer"
    Unset       -> "unset"

data JustifyContent
  = JustifyContentCenter
  | JustifyContentEnd
  | JustifyContentFlexEnd
  | JustifyContentFlexStart
  | JustifyContentLeft
  | JustifyContentNormal
  | JustifyContentRight
  | JustifyContentSafeCenter
  | JustifyContentSpaceAround
  | JustifyContentSpaceBetween
  | JustifyContentSpaceEvenly
  | JustifyContentStart
  | JustifyContentStretch
  | JustifyContentUnsafeCenter
  | JustifyContentGlobal GlobalValues

type JustifyContentImpl = String

instance ToFFI JustifyContent JustifyContentImpl where
  toNative = case _ of
    JustifyContentCenter       -> "center"
    JustifyContentEnd          -> "end"
    JustifyContentFlexEnd      -> "flex-end"
    JustifyContentFlexStart    -> "flex-start"
    JustifyContentLeft         -> "left"
    JustifyContentNormal       -> "normal"
    JustifyContentRight        -> "right"
    JustifyContentSafeCenter   -> "safe center"
    JustifyContentSpaceAround  -> "space-around"
    JustifyContentSpaceBetween -> "space-between"
    JustifyContentSpaceEvenly  -> "space-evenly"
    JustifyContentStart        -> "start"
    JustifyContentStretch      -> "stretch"
    JustifyContentUnsafeCenter -> "unsafe center"
    JustifyContentGlobal gv    -> toNative gv

data ListStyleType
  = ListStyleTypeDisc
  | ListStyleTypeCircle
  | ListStyleTypeSquare
  | ListStyleTypeDecimal
  | ListStyleTypeGeorgian
  | ListStyleTypeTradChineseInformal
  | ListStyleTypeKannada
  | ListStyleTypeCustomCounterStyle
  | ListStyleTypeNone
  | ListStyleTypeStringValue String
  | ListStyleTypeGlobal GlobalValues

type ListStyleTypeImpl = String

instance ToFFI ListStyleType ListStyleTypeImpl where
  toNative = case _ of
    ListStyleTypeDisc                -> "disc"
    ListStyleTypeCircle              -> "circle"
    ListStyleTypeSquare              -> "square"
    ListStyleTypeDecimal             -> "decimal"
    ListStyleTypeGeorgian            -> "georgian"
    ListStyleTypeTradChineseInformal -> "trad-chinese-informal"
    ListStyleTypeKannada             -> "kannada"
    ListStyleTypeCustomCounterStyle  -> "custom-counter-style"
    ListStyleTypeNone                -> "none"
    ListStyleTypeStringValue      s  -> quote s
    ListStyleTypeGlobal           gv -> toNative gv

data ObjectFit
  = ObjectFitContain
  | ObjectFitCover
  | ObjectFitFill
  | ObjectFitNone
  | ObjectFitScaleDown
  | ObjectFitGlobal GlobalValues

type ObjectFitImpl = String

instance ToFFI ObjectFit ObjectFitImpl where
  toNative = case _ of
    ObjectFitContain   -> "contain"
    ObjectFitCover     -> "cover"
    ObjectFitFill      -> "fill"
    ObjectFitNone      -> "none"
    ObjectFitScaleDown -> "scale-down"
    ObjectFitGlobal gv -> toNative gv

data PointerEvents
  = PointerEventsAll
  | PointerEventsAuto
  | PointerEventsBoundingBox
  | PointerEventsFill
  | PointerEventsNone
  | PointerEventsPainted
  | PointerEventsStroke
  | PointerEventsVisible
  | PointerEventsVisibleFill
  | PointerEventsVisiblePainted
  | PointerEventsVisibleStroke
  | PointerEventsGlobal GlobalValues

type PointerEventsImpl = String

instance ToFFI PointerEvents PointerEventsImpl where
  toNative = case _ of
    PointerEventsAll            -> "all"
    PointerEventsAuto           -> "auto"
    PointerEventsBoundingBox    -> "bounding-box"
    PointerEventsFill           -> "fill"
    PointerEventsNone           -> "none"
    PointerEventsPainted        -> "painted"
    PointerEventsStroke         -> "stroke"
    PointerEventsVisible        -> "visible"
    PointerEventsVisibleFill    -> "visibleFill"
    PointerEventsVisiblePainted -> "visiblePainted"
    PointerEventsVisibleStroke  -> "visibleStroke"
    PointerEventsGlobal gv      -> toNative gv

data Position
  = PositionStatic
  | PositionRelative
  | PositionAbsolute
  | PositionFixed
  | PositionSticky
  | PositionGlobal GlobalValues

type PositionImpl = String

instance ToFFI Position PositionImpl where
  toNative = case _ of
    PositionStatic    -> "static"
    PositionRelative  -> "relative"
    PositionAbsolute  -> "absolute"
    PositionFixed     -> "fixed"
    PositionSticky    -> "sticky"
    PositionGlobal gv -> toNative gv

data TableLayout
  = TableLayoutAuto
  | TableLayoutFixed
  | TableLayoutGlobal GlobalValues

type TableLayoutImpl = String

instance ToFFI TableLayout TableLayoutImpl where
  toNative = case _ of
    TableLayoutAuto      -> "auto"
    TableLayoutFixed     -> "fixed"
    TableLayoutGlobal gv -> toNative gv

data TextAlign
  = TextAlignCenter
  | TextAlignEnd
  | TextAlignJustifyAll
  | TextAlignJustify
  | TextAlignLeft
  | TextAlignMatchParent
  | TextAlignMozCenter
  | TextAlignRight
  | TextAlignStart
  | TextAlignWebkitCenter
  | TextAlignCharacterBased     String
  | TextAlignCharacterBasedPlus String TextAlign
  | TextAlignGlobal             GlobalValues

type TextAlignImpl = String

instance ToFFI TextAlign TextAlignImpl where
  toNative = case _ of
    TextAlignCenter                  -> "center"
    TextAlignEnd                     -> "end"
    TextAlignJustifyAll              -> "justify-all"
    TextAlignJustify                 -> "justify"
    TextAlignLeft                    -> "left"
    TextAlignMatchParent             -> "match-parent"
    TextAlignMozCenter               -> "-moz-center"
    TextAlignRight                   -> "right"
    TextAlignStart                   -> "start"
    TextAlignWebkitCenter            -> "-webkit-center"
    TextAlignCharacterBased     s    -> quote s
    TextAlignCharacterBasedPlus s ta -> quote s <> " " <> toNative ta
    TextAlignGlobal             gv   -> toNative gv

quote :: String -> String
quote s = "\"" <> s <> "\""
