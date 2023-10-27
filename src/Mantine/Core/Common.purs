module Mantine.Core.Common
  ( MantineColor(..)
  , DimmedOrColor(..)
  , MantineSize(..)
  , MantineNumberSize(..)
  , MantineNumberSizeImpl
  , Orientation(..)
  , Radius(..)
  , AlignItems(..)
  , AlignContent(..)
  , Position(..)
  , MantineGradient
  , MantineGradientImpl
  , Degrees(..)
  , JustifyContent(..)
  , Milliseconds
  , Pixels
  , Dimension(..)
  , DimensionImpl
  , MantineTransition(..)
  , MantineTransitionTimingFunction(..)
  , TextAlign(..)

  , ThemingProps
  , ThemingPropsRow
  , defaultThemingProps
  , defaultThemingProps_
  , ThemingPropsImpl
  , ThemingPropsImplRow
  , themingToImpl

  , ValueHandler(..)
  , CheckerHandler(..)
  , InputHandler(..)

  , mkComponent
  , mkComponentWithDefault
  , mkTrivialComponent

  , Polymorphic
  , PolymorphicImpl
  ) where

import Prelude hiding (bind)
import Data.Default (class DefaultValue, defaultValue)
import Data.Foldable (foldMap)
import Data.Functor.Contravariant (class Contravariant)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (Nullable)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Foreign (Foreign)
import Foreign.Object (Object)
import Mantine.FFI (class FromFFI, class ToFFI, fromNative, toNative)
import Prim.RowList (class RowToList)
import React.Basic (ReactComponent, element)
import React.Basic.Emotion (Style)
import React.Basic.Events (EventHandler, handler)
import React.Basic.DOM.Events (targetChecked, targetValue)
import React.Basic.Hooks (JSX)
import Record (merge, union)
import Type.Row (class Nub, class Union, type (+))
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

data MantineNumberSize
  = Custom Number
  | Preset MantineSize

instance ToFFI MantineNumberSize MantineNumberSizeImpl where
  toNative = case _ of
    Custom n -> asOneOf n
    Preset s -> asOneOf (toNative s)

type MantineNumberSizeImpl = Number |+| String

data Radius
  = RadiusPreset MantineSize
  | Radius Number

data Orientation = Horizontal | Vertical

instance ToFFI Orientation String where toNative = orientationNative

orientationNative :: Orientation -> String
orientationNative = case _ of
  Horizontal -> "horizontal"
  Vertical   -> "vertical"

instance ToFFI MantineSize String where toNative = sizeNative

sizeNative :: MantineSize -> String
sizeNative = case _ of
  ExtraSmall -> "xs"
  Small      -> "sm"
  Medium     -> "md"
  Large      -> "lg"
  ExtraLarge -> "xl"

instance ToFFI Radius String where toNative = radiusNative

radiusNative :: Radius -> String
radiusNative = case _ of
  Radius       nr -> show nr
  RadiusPreset s  -> toNative s

instance ToFFI MantineColor String where toNative = colorNative

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

instance ToFFI DimmedOrColor String where toNative = dimmedOrColorNative

dimmedOrColorNative :: DimmedOrColor -> String
dimmedOrColorNative = case _ of
  Dimmed  -> "dimmed"
  Color c -> toNative c

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

instance ToFFI AlignItems String where toNative = alignItemsNative

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

instance ToFFI AlignContent String where toNative = alignContentNative

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

instance ToFFI Position String where toNative = positionNative

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

instance ToFFI Degrees Number where toNative = unwrap

type MantineGradientImpl = { from :: String, to :: String, angle :: Nullable Number }

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

instance ToFFI JustifyContent String where toNative = justifyContentNative

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

data Dimension = Pixels Number | Dimension String

instance ToFFI Dimension DimensionImpl where
  toNative = case _ of
    Pixels    p -> asOneOf p
    Dimension n -> asOneOf n

type DimensionImpl = Number |+| String

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

instance ToFFI MantineTransition String where toNative = transitionNative

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

instance ToFFI MantineTransitionTimingFunction String where toNative = transitionTimingFunctionNative

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

instance ToFFI TextAlign String where toNative = textAlignNative

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

defaultThemingPropsGeneral
  :: forall otherProps
   . Nub (ThemingPropsRow otherProps)
         (ThemingPropsRow otherProps)
  => Record otherProps -> ThemingProps otherProps
defaultThemingPropsGeneral =
  let baseProps :: ThemingProps ()
      baseProps = { sx: mempty } `union` defaultValue
   in merge baseProps

defaultThemingProps
  :: forall otherProps nonDefaultableProps defaultableProps defaultablePropsList
   . Nub (ThemingPropsRow otherProps)
         (ThemingPropsRow otherProps)
  => Union nonDefaultableProps defaultableProps otherProps
  => RowToList defaultableProps defaultablePropsList
  => DefaultValue (Record defaultableProps)
  => Record nonDefaultableProps
  -> ThemingProps otherProps
defaultThemingProps nonDefaultable = defaultThemingPropsGeneral (nonDefaultable `union` defaultValue)

defaultThemingProps_
  :: forall otherProps
   . Nub (ThemingPropsRow otherProps)
         (ThemingPropsRow otherProps)
  => DefaultValue (Record otherProps)
  => ThemingProps otherProps
defaultThemingProps_ = defaultThemingPropsGeneral defaultValue

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
themingToImpl f props@{ m, mt, mb, ml, mr, mx, my, p, pt, pb, pl, pr, px, py, bg, c, sx } =
  toNative { m, mt, mb, ml, mr, mx, my, p, pt, pb, pl, pr, px, py, bg, c, sx }
    `merge` f props

newtype ValueHandler value = ValueHandler (value -> Effect Unit)
derive instance Newtype (ValueHandler value) _

instance Contravariant ValueHandler where
  cmap f vh = wrap (unwrap vh <<< f)

instance DefaultValue (ValueHandler value) where
  defaultValue = ValueHandler (const (pure unit))

instance FromFFI nativeValue value => ToFFI (ValueHandler value) (EffectFn1 nativeValue Unit) where
  toNative (ValueHandler vh) = mkEffectFn1 (vh <<< fromNative)

newtype CheckerHandler = CheckerHandler (Boolean -> Effect Unit)
derive instance Newtype CheckerHandler _

instance DefaultValue CheckerHandler where
  defaultValue = CheckerHandler (const (pure unit))

instance ToFFI CheckerHandler EventHandler where
  toNative (CheckerHandler ch) = handler targetChecked (foldMap ch)

newtype InputHandler = InputHandler (String -> Effect Unit)
derive instance Newtype InputHandler _

instance DefaultValue InputHandler where
  defaultValue = InputHandler (const (pure unit))

instance ToFFI InputHandler EventHandler where
  toNative (InputHandler ch) = handler targetValue (foldMap ch)

mkComponent :: forall props foreignProps. ReactComponent (Record foreignProps) -> (props -> Record foreignProps) -> props -> (props -> props) -> JSX
mkComponent cmpt converter default setProps = element cmpt (converter (setProps default))

mkComponentWithDefault :: forall props foreignProps. ToFFI props (Record foreignProps) => ReactComponent (Record foreignProps) -> props -> (props -> props) -> JSX
mkComponentWithDefault cmpt = mkComponent cmpt toNative

mkTrivialComponent :: forall props foreignProps
                    . Nub (ThemingPropsRow props) (ThemingPropsRow props)
                   => DefaultValue (Record props)
                   => ToFFI (ThemingProps props) (ThemingPropsImpl foreignProps)
                   => ReactComponent (ThemingPropsImpl foreignProps) -> (ThemingProps props -> ThemingProps props) -> JSX
mkTrivialComponent cmpt = mkComponentWithDefault cmpt defaultThemingProps_

type Polymorphic rest =
  ( component        :: Maybe String
  , polymorphicProps :: Object Foreign
  | rest
  )

type PolymorphicImpl rest =
  ( component        :: Nullable String
  , polymorphicProps :: Object Foreign
  | rest
  )
