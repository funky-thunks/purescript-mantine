module Mantine.Core.Common
  ( MantineColor(..)
  , colorNative
  , DimmedOrColor(..)
  , dimmedOrColorNative
  , MantineSize(..)
  , sizeNative
  , MantineNumberSize
  , MantineNumberSizeImpl
  , numberSizeNative
  , Orientation(..)
  , orientationNative
  , Radius(..)
  , radiusNative
  , AlignItems(..)
  , alignItemsNative
  , AlignContent(..)
  , alignContentNative
  , Position(..)
  , positionNative
  , MantineGradient
  , MantineGradientImpl
  , gradientNative
  , Degrees(..)
  , JustifyContent(..)
  , justifyContentNative
  , Milliseconds
  , Pixels
  , Dimension
  , DimensionImpl
  , dimensionNative
  , MantineTransition(..)
  , transitionNative
  , MantineTransitionTimingFunction(..)
  , transitionTimingFunctionNative
  , TextAlign(..)
  , textAlignNative

  , ThemingProps
  , ThemingPropsRow
  , defaultThemingProps
  , ThemingPropsImpl
  , ThemingPropsImplRow
  , themingToImpl
  ) where

import Prelude hiding (bind)
import Data.Either (Either, either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Nullable (Nullable, toNullable)
import Data.Show.Generic (genericShow)
import React.Basic.Emotion (Style)
import Record (merge)
import Type.Row (class Nub, type (+))
import Untagged.Union (type (|+|), asOneOf)

data MantineColor
  = Dark
  | Gray
  | Red
  | Pink
  | Grape
  | Violet
  | Indigo
  | Blue
  | Cyan
  | Teal
  | Green
  | Lime
  | Yellow
  | Orange

  | Dark0
  | Gray0
  | Red0
  | Pink0
  | Grape0
  | Violet0
  | Indigo0
  | Blue0
  | Cyan0
  | Teal0
  | Green0
  | Lime0
  | Yellow0
  | Orange0

  | Dark1
  | Gray1
  | Red1
  | Pink1
  | Grape1
  | Violet1
  | Indigo1
  | Blue1
  | Cyan1
  | Teal1
  | Green1
  | Lime1
  | Yellow1
  | Orange1

  | Dark2
  | Gray2
  | Red2
  | Pink2
  | Grape2
  | Violet2
  | Indigo2
  | Blue2
  | Cyan2
  | Teal2
  | Green2
  | Lime2
  | Yellow2
  | Orange2

  | Dark3
  | Gray3
  | Red3
  | Pink3
  | Grape3
  | Violet3
  | Indigo3
  | Blue3
  | Cyan3
  | Teal3
  | Green3
  | Lime3
  | Yellow3
  | Orange3

  | Dark4
  | Gray4
  | Red4
  | Pink4
  | Grape4
  | Violet4
  | Indigo4
  | Blue4
  | Cyan4
  | Teal4
  | Green4
  | Lime4
  | Yellow4
  | Orange4

  | Dark5
  | Gray5
  | Red5
  | Pink5
  | Grape5
  | Violet5
  | Indigo5
  | Blue5
  | Cyan5
  | Teal5
  | Green5
  | Lime5
  | Yellow5
  | Orange5

  | Dark6
  | Gray6
  | Red6
  | Pink6
  | Grape6
  | Violet6
  | Indigo6
  | Blue6
  | Cyan6
  | Teal6
  | Green6
  | Lime6
  | Yellow6
  | Orange6

  | Dark7
  | Gray7
  | Red7
  | Pink7
  | Grape7
  | Violet7
  | Indigo7
  | Blue7
  | Cyan7
  | Teal7
  | Green7
  | Lime7
  | Yellow7
  | Orange7

  | Dark8
  | Gray8
  | Red8
  | Pink8
  | Grape8
  | Violet8
  | Indigo8
  | Blue8
  | Cyan8
  | Teal8
  | Green8
  | Lime8
  | Yellow8
  | Orange8

  | Dark9
  | Gray9
  | Red9
  | Pink9
  | Grape9
  | Violet9
  | Indigo9
  | Blue9
  | Cyan9
  | Teal9
  | Green9
  | Lime9
  | Yellow9
  | Orange9

  | Hexa String

derive instance genericMantineColor :: Generic MantineColor _
instance showMantineColor :: Show MantineColor where show = genericShow

data DimmedOrColor
  = Dimmed
  | Color MantineColor

data MantineSize
  = ExtraSmall
  | Small
  | Medium
  | Large
  | ExtraLarge

type MantineNumberSize = Either Number MantineSize
type MantineNumberSizeImpl = Number |+| String

data Radius
  = RadiusPreset MantineSize
  | Radius Number

data Orientation = Horizontal | Vertical

sizeNative :: MantineSize -> String
sizeNative = case _ of
  ExtraSmall -> "xs"
  Small      -> "sm"
  Medium     -> "md"
  Large      -> "lg"
  ExtraLarge -> "xl"

numberSizeNative :: MantineNumberSize -> MantineNumberSizeImpl
numberSizeNative = either asOneOf (asOneOf <<< sizeNative)

radiusNative :: Radius -> String
radiusNative = case _ of
  Radius       nr -> show nr
  RadiusPreset s  -> sizeNative s

colorNative :: MantineColor -> String
colorNative = case _ of
  Dark   -> "dark"
  Gray   -> "gray"
  Red    -> "red"
  Pink   -> "pink"
  Grape  -> "grape"
  Violet -> "violet"
  Indigo -> "indigo"
  Blue   -> "blue"
  Cyan   -> "cyan"
  Teal   -> "teal"
  Green  -> "green"
  Lime   -> "lime"
  Yellow -> "yellow"
  Orange -> "orange"

  Dark0   -> "dark.0"
  Gray0   -> "gray.0"
  Red0    -> "red.0"
  Pink0   -> "pink.0"
  Grape0  -> "grape.0"
  Violet0 -> "violet.0"
  Indigo0 -> "indigo.0"
  Blue0   -> "blue.0"
  Cyan0   -> "cyan.0"
  Teal0   -> "teal.0"
  Green0  -> "green.0"
  Lime0   -> "lime.0"
  Yellow0 -> "yellow.0"
  Orange0 -> "orange.0"

  Dark1   -> "dark.1"
  Gray1   -> "gray.1"
  Red1    -> "red.1"
  Pink1   -> "pink.1"
  Grape1  -> "grape.1"
  Violet1 -> "violet.1"
  Indigo1 -> "indigo.1"
  Blue1   -> "blue.1"
  Cyan1   -> "cyan.1"
  Teal1   -> "teal.1"
  Green1  -> "green.1"
  Lime1   -> "lime.1"
  Yellow1 -> "yellow.1"
  Orange1 -> "orange.1"

  Dark2   -> "dark.2"
  Gray2   -> "gray.2"
  Red2    -> "red.2"
  Pink2   -> "pink.2"
  Grape2  -> "grape.2"
  Violet2 -> "violet.2"
  Indigo2 -> "indigo.2"
  Blue2   -> "blue.2"
  Cyan2   -> "cyan.2"
  Teal2   -> "teal.2"
  Green2  -> "green.2"
  Lime2   -> "lime.2"
  Yellow2 -> "yellow.2"
  Orange2 -> "orange.2"

  Dark3   -> "dark.3"
  Gray3   -> "gray.3"
  Red3    -> "red.3"
  Pink3   -> "pink.3"
  Grape3  -> "grape.3"
  Violet3 -> "violet.3"
  Indigo3 -> "indigo.3"
  Blue3   -> "blue.3"
  Cyan3   -> "cyan.3"
  Teal3   -> "teal.3"
  Green3  -> "green.3"
  Lime3   -> "lime.3"
  Yellow3 -> "yellow.3"
  Orange3 -> "orange.3"

  Dark4   -> "dark.4"
  Gray4   -> "gray.4"
  Red4    -> "red.4"
  Pink4   -> "pink.4"
  Grape4  -> "grape.4"
  Violet4 -> "violet.4"
  Indigo4 -> "indigo.4"
  Blue4   -> "blue.4"
  Cyan4   -> "cyan.4"
  Teal4   -> "teal.4"
  Green4  -> "green.4"
  Lime4   -> "lime.4"
  Yellow4 -> "yellow.4"
  Orange4 -> "orange.4"

  Dark5   -> "dark.5"
  Gray5   -> "gray.5"
  Red5    -> "red.5"
  Pink5   -> "pink.5"
  Grape5  -> "grape.5"
  Violet5 -> "violet.5"
  Indigo5 -> "indigo.5"
  Blue5   -> "blue.5"
  Cyan5   -> "cyan.5"
  Teal5   -> "teal.5"
  Green5  -> "green.5"
  Lime5   -> "lime.5"
  Yellow5 -> "yellow.5"
  Orange5 -> "orange.5"

  Dark6   -> "dark.6"
  Gray6   -> "gray.6"
  Red6    -> "red.6"
  Pink6   -> "pink.6"
  Grape6  -> "grape.6"
  Violet6 -> "violet.6"
  Indigo6 -> "indigo.6"
  Blue6   -> "blue.6"
  Cyan6   -> "cyan.6"
  Teal6   -> "teal.6"
  Green6  -> "green.6"
  Lime6   -> "lime.6"
  Yellow6 -> "yellow.6"
  Orange6 -> "orange.6"

  Dark7   -> "dark.7"
  Gray7   -> "gray.7"
  Red7    -> "red.7"
  Pink7   -> "pink.7"
  Grape7  -> "grape.7"
  Violet7 -> "violet.7"
  Indigo7 -> "indigo.7"
  Blue7   -> "blue.7"
  Cyan7   -> "cyan.7"
  Teal7   -> "teal.7"
  Green7  -> "green.7"
  Lime7   -> "lime.7"
  Yellow7 -> "yellow.7"
  Orange7 -> "orange.7"

  Dark8   -> "dark.8"
  Gray8   -> "gray.8"
  Red8    -> "red.8"
  Pink8   -> "pink.8"
  Grape8  -> "grape.8"
  Violet8 -> "violet.8"
  Indigo8 -> "indigo.8"
  Blue8   -> "blue.8"
  Cyan8   -> "cyan.8"
  Teal8   -> "teal.8"
  Green8  -> "green.8"
  Lime8   -> "lime.8"
  Yellow8 -> "yellow.8"
  Orange8 -> "orange.8"

  Dark9   -> "dark.9"
  Gray9   -> "gray.9"
  Red9    -> "red.9"
  Pink9   -> "pink.9"
  Grape9  -> "grape.9"
  Violet9 -> "violet.9"
  Indigo9 -> "indigo.9"
  Blue9   -> "blue.9"
  Cyan9   -> "cyan.9"
  Teal9   -> "teal.9"
  Green9  -> "green.9"
  Lime9   -> "lime.9"
  Yellow9 -> "yellow.9"
  Orange9 -> "orange.9"

  Hexa h -> "#" <> h

dimmedOrColorNative :: DimmedOrColor -> String
dimmedOrColorNative = case _ of
  Dimmed  -> "dimmed"
  Color c -> colorNative c

orientationNative :: Orientation -> String
orientationNative = case _ of
  Horizontal -> "horizontal"
  Vertical   -> "vertical"

data AlignItems
  = AlignItemsNormal
  | AlignItemsStretch
  | AlignItemsCenter
  | AlignItemsStart
  | AlignItemsEnd
  | AlignItemsFlexStart
  | AlignItemsFlexEnd
  | AlignItemsBaseline
  | AlignItemsFirstBaseline
  | AlignItemsLastBaseline
  | AlignItemsSafeCenter
  | AlignItemsUnsafeCenter
  | AlignItemsInherit
  | AlignItemsInitial
  | AlignItemsRevert
  | AlignItemsRevertLayer
  | AlignItemsUnset

alignItemsNative :: AlignItems -> String
alignItemsNative = case _ of
  AlignItemsNormal        -> "normal"
  AlignItemsStretch       -> "stretch"
  AlignItemsCenter        -> "center"
  AlignItemsStart         -> "start"
  AlignItemsEnd           -> "end"
  AlignItemsFlexStart     -> "flex-start"
  AlignItemsFlexEnd       -> "flex-end"
  AlignItemsBaseline      -> "baseline"
  AlignItemsFirstBaseline -> "first baseline"
  AlignItemsLastBaseline  -> "last baseline"
  AlignItemsSafeCenter    -> "safe center"
  AlignItemsUnsafeCenter  -> "unsafe center"
  AlignItemsInherit       -> "inherit"
  AlignItemsInitial       -> "initial"
  AlignItemsRevert        -> "revert"
  AlignItemsRevertLayer   -> "revert-layer"
  AlignItemsUnset         -> "unset"

data AlignContent
  = AlignContentCenter
  | AlignContentStart
  | AlignContentEnd
  | AlignContentFlexStart
  | AlignContentFlexEnd
  | AlignContentNormal
  | AlignContentBaseline
  | AlignContentFirstBaseline
  | AlignContentLastBaseline
  | AlignContentSpaceBetween
  | AlignContentSpaceAround
  | AlignContentSpaceEvenly
  | AlignContentStretch
  | AlignContentSafeCenter
  | AlignContentUnsafeCenter
  | AlignContentInherit
  | AlignContentInitial
  | AlignContentRevert
  | AlignContentRevertLayer
  | AlignContentUnset

alignContentNative :: AlignContent -> String
alignContentNative = case _ of
  AlignContentCenter        -> "center"
  AlignContentStart         -> "start"
  AlignContentEnd           -> "end"
  AlignContentFlexStart     -> "flex-start"
  AlignContentFlexEnd       -> "flex-end"
  AlignContentNormal        -> "normal"
  AlignContentBaseline      -> "baseline"
  AlignContentFirstBaseline -> "first baseline"
  AlignContentLastBaseline  -> "last baseline"
  AlignContentSpaceBetween  -> "space-between"
  AlignContentSpaceAround   -> "space-around"
  AlignContentSpaceEvenly   -> "space-evenly"
  AlignContentStretch       -> "stretch"
  AlignContentSafeCenter    -> "safe center"
  AlignContentUnsafeCenter  -> "unsafe center"
  AlignContentInherit       -> "inherit"
  AlignContentInitial       -> "initial"
  AlignContentRevert        -> "revert"
  AlignContentRevertLayer   -> "revert-layer"
  AlignContentUnset         -> "unset"

data Position
  = PositionLeft
  | PositionRight
  | PositionCenter
  | PositionApart

positionNative :: Position -> String
positionNative = case _ of
  PositionLeft   -> "left"
  PositionRight  -> "right"
  PositionCenter -> "center"
  PositionApart  -> "apart"

type MantineGradient
  = { from  :: MantineColor
    , to    :: MantineColor
    , angle :: Maybe Degrees
    }

newtype Degrees = Degrees Number

derive instance newtypeDegrees :: Newtype Degrees _
derive newtype instance showDegrees :: Show Degrees

type MantineGradientImpl = { from :: String, to :: String, angle :: Nullable Number }

gradientNative :: MantineGradient -> MantineGradientImpl
gradientNative { from, to, angle } =
  { from:  colorNative from
  , to:    colorNative to
  , angle: toNullable (unwrap <$> angle)
  }

data JustifyContent
  = JustifyContentCenter
  | JustifyContentStart
  | JustifyContentEnd
  | JustifyContentFlexStart
  | JustifyContentFlexEnd
  | JustifyContentLeft
  | JustifyContentRight
  | JustifyContentNormal
  | JustifyContentSpaceBetween
  | JustifyContentSpaceAround
  | JustifyContentSpaceEvenly
  | JustifyContentStretch
  | JustifyContentSafeCenter
  | JustifyContentUnsafeCenter
  | JustifyContentInherit
  | JustifyContentInitial
  | JustifyContentRevert
  | JustifyContentRevertLayer
  | JustifyContentUnset

justifyContentNative :: JustifyContent -> String
justifyContentNative = case _ of
  JustifyContentCenter       -> "center"
  JustifyContentStart        -> "start"
  JustifyContentEnd          -> "end"
  JustifyContentFlexStart    -> "flex-start"
  JustifyContentFlexEnd      -> "flex-end"
  JustifyContentLeft         -> "left"
  JustifyContentRight        -> "right"
  JustifyContentNormal       -> "normal"
  JustifyContentSpaceBetween -> "space-between"
  JustifyContentSpaceAround  -> "space-around"
  JustifyContentSpaceEvenly  -> "space-evenly"
  JustifyContentStretch      -> "stretch"
  JustifyContentSafeCenter   -> "safe center"
  JustifyContentUnsafeCenter -> "unsafe center"
  JustifyContentInherit      -> "inherit"
  JustifyContentInitial      -> "initial"
  JustifyContentRevert       -> "revert"
  JustifyContentRevertLayer  -> "revert-layer"
  JustifyContentUnset        -> "unset"

type Milliseconds = Number
type Pixels = Number

type Dimension = Either Number String
type DimensionImpl = Number |+| String

dimensionNative :: Dimension -> DimensionImpl
dimensionNative = either asOneOf asOneOf

data MantineTransition
  = TransitionFade
  | TransitionPop
  | TransitionPopBottomLeft
  | TransitionPopBottomRight
  | TransitionPopTopLeft
  | TransitionPopTopRight
  | TransitionRotateLeft
  | TransitionRotateRight
  | TransitionScale
  | TransitionScaleX
  | TransitionScaleY
  | TransitionSkewDown
  | TransitionSkewUp
  | TransitionSlideDown
  | TransitionSlideLeft
  | TransitionSlideRight
  | TransitionSlideUp

transitionNative :: MantineTransition -> String
transitionNative = case _ of
  TransitionFade           -> "fade"
  TransitionPop            -> "pop"
  TransitionPopBottomLeft  -> "pop-bottom-left"
  TransitionPopBottomRight -> "pop-bottom-right"
  TransitionPopTopLeft     -> "pop-top-left"
  TransitionPopTopRight    -> "pop-top-right"
  TransitionRotateLeft     -> "rotate-left"
  TransitionRotateRight    -> "rotate-right"
  TransitionScale          -> "scale"
  TransitionScaleX         -> "scale-x"
  TransitionScaleY         -> "scale-y"
  TransitionSkewDown       -> "skew-down"
  TransitionSkewUp         -> "skew-up"
  TransitionSlideDown      -> "slide-down"
  TransitionSlideLeft      -> "slide-left"
  TransitionSlideRight     -> "slide-right"
  TransitionSlideUp        -> "slide-up"

data MantineTransitionTimingFunction
  = TransitionTimingEase
  | TransitionTimingLinear

transitionTimingFunctionNative :: MantineTransitionTimingFunction -> String
transitionTimingFunctionNative = case _ of
  TransitionTimingEase   -> "ease"
  TransitionTimingLinear -> "linear"

data TextAlign
  = TextAlignLeft
  | TextAlignRight
  | TextAlignMozInitial
  | TextAlignInherit
  | TextAlignInitial
  | TextAlignRevert
  | TextAlignUnset
  | TextAlignCenter
  | TextAlignEnd
  | TextAlignStart
  | TextAlignJustify
  | TextAlignMatchParent

textAlignNative :: TextAlign -> String
textAlignNative = case _ of
  TextAlignLeft        -> "left"
  TextAlignRight       -> "right"
  TextAlignMozInitial  -> "-moz-initial"
  TextAlignInherit     -> "inherit"
  TextAlignInitial     -> "initial"
  TextAlignRevert      -> "revert"
  TextAlignUnset       -> "unset"
  TextAlignCenter      -> "center"
  TextAlignEnd         -> "end"
  TextAlignStart       -> "start"
  TextAlignJustify     -> "justify"
  TextAlignMatchParent -> "match-parent"

type ThemingProps r = Record (ThemingPropsRow + r)

type ThemingPropsRow r =
  ( m  :: Maybe MantineSize
  , mt :: Maybe MantineSize
  , mb :: Maybe MantineSize
  , ml :: Maybe MantineSize
  , mr :: Maybe MantineSize
  , mx :: Maybe MantineSize
  , my :: Maybe MantineSize
  , p  :: Maybe MantineSize
  , pt :: Maybe MantineSize
  , pb :: Maybe MantineSize
  , pl :: Maybe MantineSize
  , pr :: Maybe MantineSize
  , px :: Maybe MantineSize
  , py :: Maybe MantineSize
  , bg :: Maybe MantineColor
  , c  :: Maybe MantineColor
  , sx :: Style
  | r
  )

defaultThemingProps
  :: forall otherProps
   . Nub (ThemingPropsRow otherProps)
         (ThemingPropsRow otherProps)
  => Record otherProps -> ThemingProps otherProps
defaultThemingProps defaults =
  let baseProps :: ThemingProps ()
      baseProps =
        { m:  Nothing
        , mt: Nothing
        , mb: Nothing
        , ml: Nothing
        , mr: Nothing
        , mx: Nothing
        , my: Nothing
        , p:  Nothing
        , pt: Nothing
        , pb: Nothing
        , pl: Nothing
        , pr: Nothing
        , px: Nothing
        , py: Nothing
        , bg: Nothing
        , c:  Nothing
        , sx: mempty
        }
   in baseProps `merge` defaults

type ThemingPropsImpl otherProps = Record (ThemingPropsImplRow + otherProps)

type ThemingPropsImplRow r =
  ( m  :: Nullable String
  , mt :: Nullable String
  , mb :: Nullable String
  , ml :: Nullable String
  , mr :: Nullable String
  , mx :: Nullable String
  , my :: Nullable String
  , p  :: Nullable String
  , pt :: Nullable String
  , pb :: Nullable String
  , pl :: Nullable String
  , pr :: Nullable String
  , px :: Nullable String
  , py :: Nullable String
  , bg :: Nullable String
  , c  :: Nullable String
  , sx :: Style
  | r
  )

themingToImpl :: forall otherProps otherPropsImpl
               . Nub (ThemingPropsImplRow otherPropsImpl)
                     (ThemingPropsImplRow otherPropsImpl)
              => (ThemingProps otherProps -> Record                      otherPropsImpl)
              ->  ThemingProps otherProps -> Record (ThemingPropsImplRow otherPropsImpl)
themingToImpl f props@{ sx } =
  let toSize  f' = toNullable $ sizeNative  <$> f' props
      toColor f' = toNullable $ colorNative <$> f' props
   in { m:  toSize  _.m
      , mt: toSize  _.mt
      , mb: toSize  _.mb
      , ml: toSize  _.ml
      , mr: toSize  _.mr
      , mx: toSize  _.mx
      , my: toSize  _.my
      , p:  toSize  _.p
      , pt: toSize  _.pt
      , pb: toSize  _.pb
      , pl: toSize  _.pl
      , pr: toSize  _.pr
      , px: toSize  _.px
      , py: toSize  _.py
      , bg: toColor _.bg
      , c:  toColor _.c
      , sx
      } `merge` f props
