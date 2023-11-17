module Mantine.Core.Common
  ( MantineColor(..)
  , MantineColorImpl
  , DimmedOrColor(..)
  , DimmedOrColorImpl
  , MantineSize(..)
  , MantineSizeImpl
  , MantineShadow
  , MantineShadowImpl
  , MantineNumberSize(..)
  , MantineNumberSizeImpl
  , Orientation(..)
  , OrientationImpl
  , Radius(..)
  , RadiusImpl
  , MantineGradient
  , MantineGradientImpl
  , Degrees(..)
  , DegreesImpl
  , Milliseconds
  , MillisecondsImpl
  , Pixels
  , PixelsImpl
  , Rem
  , RemImpl
  , ZIndex
  , ZIndexImpl
  , Dimension(..)
  , DimensionImpl
  , MantineTransitionProps
  , MantineTransitionPropsImpl
  , MantineTransitionBase
  , MantineTransitionBaseImpl
  , MantineTransition(..)
  , MantineTransitionImpl
  , MantineTransitionTimingFunction(..)
  , MantineTransitionTimingFunctionImpl
  , MantineSpacing
  , MantineSpacingImpl
  , PopoverMiddlewares
  , PopoverMiddlewaresImpl

  , MantineComponent
  , MantineComponentRow
  , defaultMantineComponent
  , defaultMantineComponent_
  , MantineComponentImpl
  , MantineComponentImplRow

  , ValueHandler(..)
  , ValueHandlerImpl
  , CheckerHandler(..)
  , CheckerHandlerImpl
  , InputHandler(..)
  , InputHandlerImpl

  , mkComponent
  , mkComponentWithDefault
  , mkTrivialComponent

  , Polymorphic
  , PolymorphicImpl

  , Responsive
  , ResponsiveImpl
  , FixedOrResponsive
  , FixedOrResponsiveImpl

  , Controlled
  , Controlled_
  , ControlledImpl
  , ControlledImpl_

  , Breakpoint(..)
  , BreakpointImpl
  ) where

import Prelude hiding (bind)
import Data.Default (class DefaultValue, defaultValue)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Functor.Contravariant (class Contravariant)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Natural (Natural)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Number (fromString)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), stripSuffix)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1, runEffectFn1)
import Foreign (Foreign)
import Foreign.Object (Object)
import Mantine.Core.CSS (FontWeight, FontWeightImpl)
import Mantine.FFI (class FromFFI, class ToFFI, Optional, OptionalImpl, fromNative, toNative)
import Prim.RowList (class RowToList)
import React.Basic (ReactComponent, element)
import React.Basic.Events (EventHandler, handler)
import React.Basic.DOM (CSS)
import React.Basic.DOM.Events (targetChecked, targetValue)
import React.Basic.Hooks (JSX)
import Record (merge, union)
import Type.Row (class Nub, class Union, type (+))
import Untagged.Union (type (|+|), asOneOf, toEither1)

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

  | Named        String
  | NamedVariant String Natural

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
  = Custom Pixels
  | InRems Rem
  | Preset MantineSize

type MantineShadow = MantineSize
type MantineShadowImpl = MantineSizeImpl

instance ToFFI MantineNumberSize MantineNumberSizeImpl where
  toNative = case _ of
    Custom n -> asOneOf n
    InRems r -> asOneOf (show r <> "rem")
    Preset s -> asOneOf (toNative s)

instance FromFFI MantineNumberSizeImpl MantineNumberSize where
  fromNative = toEither1 >>> case _ of
    Left  n -> Custom n
    Right s ->
      case stripSuffix (Pattern "rem") s >>= fromString of
        Just r  -> InRems r
        Nothing -> Preset (fromNative s)

type MantineNumberSizeImpl = Number |+| MantineSizeImpl

data Radius
  = RadiusPreset MantineSize
  | Radius Number

data Orientation = Horizontal | Vertical

type OrientationImpl = String

instance ToFFI Orientation OrientationImpl where
  toNative = case _ of
    Horizontal -> "horizontal"
    Vertical   -> "vertical"

type MantineSizeImpl = String

instance ToFFI MantineSize MantineSizeImpl where
  toNative = case _ of
    ExtraSmall -> "xs"
    Small      -> "sm"
    Medium     -> "md"
    Large      -> "lg"
    ExtraLarge -> "xl"

instance FromFFI String MantineSize where
  fromNative = case _ of
    "xs" -> ExtraSmall
    "sm" -> Small
    "md" -> Medium
    "lg" -> Large
    "xl" -> ExtraLarge
    _    -> Medium -- This is a bad default but we need one...

type RadiusImpl = String

instance ToFFI Radius RadiusImpl where
  toNative = case _ of
    Radius       nr -> show nr
    RadiusPreset s  -> toNative s

type MantineColorImpl = String

instance ToFFI MantineColor MantineColorImpl where
  toNative = case _ of
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

    Named        n   -> n
    NamedVariant n v -> n <> "." <> show v

    Hexa h -> "#" <> h

type DimmedOrColorImpl = String

instance ToFFI DimmedOrColor DimmedOrColorImpl where
  toNative = case _ of
    Dimmed  -> "dimmed"
    Color c -> toNative c

type MantineGradient =
  { from  :: MantineColor
  , to    :: MantineColor
  , angle :: Optional Degrees
  }

newtype Degrees = Degrees Number

derive instance newtypeDegrees :: Newtype Degrees _
derive newtype instance showDegrees :: Show Degrees

type DegreesImpl = Number

instance ToFFI Degrees DegreesImpl where toNative = unwrap

type MantineGradientImpl =
  { from  :: String
  , to    :: String
  , angle :: OptionalImpl Number
  }

type Milliseconds = Number
type MillisecondsImpl = Milliseconds

type Pixels = Number
type PixelsImpl = Pixels

type Rem = Number
type RemImpl = Rem

type ZIndex = Number
type ZIndexImpl = ZIndex

data Dimension = Pixels Pixels | Rem Rem | Dimension String

instance ToFFI Dimension DimensionImpl where
  toNative = case _ of
    Pixels    p -> asOneOf p
    Rem       r -> asOneOf (show r <> "rem")
    Dimension n -> asOneOf n

type DimensionImpl = Number |+| String

type MantineTransitionProps = MantineTransitionBase ()
type MantineTransitionBase rest =
  { transition     :: Optional MantineTransition
  , duration       :: Optional Milliseconds
  , timingFunction :: Optional MantineTransitionTimingFunction
  | rest
  }

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

type MantineTransitionImpl = String

instance ToFFI MantineTransition MantineTransitionImpl where
  toNative = case _ of
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

type MantineTransitionTimingFunctionImpl = String

instance ToFFI MantineTransitionTimingFunction MantineTransitionTimingFunctionImpl where
  toNative = case _ of
    TransitionTimingEase   -> "ease"
    TransitionTimingLinear -> "linear"

type MantineTransitionPropsImpl = MantineTransitionBaseImpl ()
type MantineTransitionBaseImpl rest =
  { transition     :: OptionalImpl String
  , duration       :: OptionalImpl Number
  , timingFunction :: OptionalImpl String
  | rest
  }

type MantineSpacing     = MantineNumberSize
type MantineSpacingImpl = MantineNumberSizeImpl

type MantineComponent r = Record (MantineComponentRow + r)

type MantineComponentRow r =
  ( m           :: Optional MantineSize
  , mt          :: Optional MantineSize
  , mb          :: Optional MantineSize
  , ml          :: Optional MantineSize
  , mr          :: Optional MantineSize
  , mx          :: Optional MantineSize
  , my          :: Optional MantineSize
  , p           :: Optional MantineSize
  , pt          :: Optional MantineSize
  , pb          :: Optional MantineSize
  , pl          :: Optional MantineSize
  , pr          :: Optional MantineSize
  , px          :: Optional MantineSize
  , py          :: Optional MantineSize
  , w           :: Optional MantineSize
  , miw         :: Optional MantineSize
  , maw         :: Optional MantineSize
  , h           :: Optional MantineSize
  , mih         :: Optional MantineSize
  , mah         :: Optional MantineSize
  , fw          :: Optional FontWeight
  , bg          :: Optional MantineColor
  , c           :: Optional MantineColor
  , className   :: Optional String
  , style       :: Optional CSS
  , darkHidden  :: Optional Boolean
  , lightHidden :: Optional Boolean
  , hiddenFrom  :: Optional Breakpoint
  , visibleFrom :: Optional Breakpoint
  | r
  )

defaultMantineComponentGeneral
  :: forall otherProps
   . Nub (MantineComponentRow otherProps)
         (MantineComponentRow otherProps)
  => Record otherProps -> MantineComponent otherProps
defaultMantineComponentGeneral =
  let baseProps :: MantineComponent ()
      baseProps = defaultValue
   in merge baseProps

defaultMantineComponent
  :: forall otherProps nonDefaultableProps defaultableProps defaultablePropsList
   . Nub (MantineComponentRow otherProps)
         (MantineComponentRow otherProps)
  => Union nonDefaultableProps defaultableProps otherProps
  => RowToList defaultableProps defaultablePropsList
  => DefaultValue (Record defaultableProps)
  => Record nonDefaultableProps
  -> MantineComponent otherProps
defaultMantineComponent nonDefaultable = defaultMantineComponentGeneral (nonDefaultable `union` defaultValue)

defaultMantineComponent_
  :: forall otherProps
   . Nub (MantineComponentRow otherProps)
         (MantineComponentRow otherProps)
  => DefaultValue (Record otherProps)
  => MantineComponent otherProps
defaultMantineComponent_ = defaultMantineComponentGeneral defaultValue

type MantineComponentImpl otherProps = Record (MantineComponentImplRow + otherProps)

type MantineComponentImplRow r =
  ( m           :: OptionalImpl MantineSizeImpl
  , mt          :: OptionalImpl MantineSizeImpl
  , mb          :: OptionalImpl MantineSizeImpl
  , ml          :: OptionalImpl MantineSizeImpl
  , mr          :: OptionalImpl MantineSizeImpl
  , mx          :: OptionalImpl MantineSizeImpl
  , my          :: OptionalImpl MantineSizeImpl
  , p           :: OptionalImpl MantineSizeImpl
  , pt          :: OptionalImpl MantineSizeImpl
  , pb          :: OptionalImpl MantineSizeImpl
  , pl          :: OptionalImpl MantineSizeImpl
  , pr          :: OptionalImpl MantineSizeImpl
  , px          :: OptionalImpl MantineSizeImpl
  , py          :: OptionalImpl MantineSizeImpl
  , w           :: OptionalImpl MantineSizeImpl
  , miw         :: OptionalImpl MantineSizeImpl
  , maw         :: OptionalImpl MantineSizeImpl
  , h           :: OptionalImpl MantineSizeImpl
  , mih         :: OptionalImpl MantineSizeImpl
  , mah         :: OptionalImpl MantineSizeImpl
  , fw          :: OptionalImpl FontWeightImpl
  , bg          :: OptionalImpl MantineColorImpl
  , c           :: OptionalImpl MantineColorImpl
  , className   :: OptionalImpl String
  , style       :: OptionalImpl CSS
  , darkHidden  :: OptionalImpl Boolean
  , lightHidden :: OptionalImpl Boolean
  , hiddenFrom  :: OptionalImpl BreakpointImpl
  , visibleFrom :: OptionalImpl BreakpointImpl
  | r
  )

newtype ValueHandler value = ValueHandler (value -> Effect Unit)
derive instance Newtype (ValueHandler value) _

instance Contravariant ValueHandler where
  cmap f vh = wrap (unwrap vh <<< f)

instance DefaultValue (ValueHandler value) where
  defaultValue = ValueHandler (const (pure unit))

type ValueHandlerImpl nativeValue = EffectFn1 nativeValue Unit

instance FromFFI nativeValue value => ToFFI (ValueHandler value) (ValueHandlerImpl nativeValue) where
  toNative (ValueHandler vh) = mkEffectFn1 (vh <<< fromNative)

instance ToFFI value nativeValue => FromFFI (ValueHandlerImpl nativeValue) (ValueHandler value) where
  fromNative vh = ValueHandler (runEffectFn1 vh <<< toNative)

newtype CheckerHandler = CheckerHandler (Boolean -> Effect Unit)
derive instance Newtype CheckerHandler _

instance DefaultValue CheckerHandler where
  defaultValue = CheckerHandler (const (pure unit))

type CheckerHandlerImpl = EventHandler

instance ToFFI CheckerHandler CheckerHandlerImpl where
  toNative (CheckerHandler ch) = handler targetChecked (foldMap ch)

newtype InputHandler = InputHandler (String -> Effect Unit)
derive instance Newtype InputHandler _

instance DefaultValue InputHandler where
  defaultValue = InputHandler (const (pure unit))

type InputHandlerImpl = EventHandler

instance ToFFI InputHandler InputHandlerImpl where
  toNative (InputHandler ch) = handler targetValue (foldMap ch)

mkComponent :: forall props foreignProps
             . ReactComponent (Record foreignProps)
            -> (props -> Record foreignProps)
            -> props
            -> (props -> props)
            -> JSX
mkComponent cmpt converter default setProps = element cmpt (converter (setProps default))

mkComponentWithDefault :: forall props foreignProps
                        . ToFFI props (Record foreignProps)
                       => ReactComponent (Record foreignProps)
                       -> props
                       -> (props -> props)
                       -> JSX
mkComponentWithDefault cmpt = mkComponent cmpt toNative

mkTrivialComponent :: forall props foreignProps
                    . Nub (MantineComponentRow props) (MantineComponentRow props)
                   => DefaultValue (Record props)
                   => ToFFI (MantineComponent props) (MantineComponentImpl foreignProps)
                   => ReactComponent (MantineComponentImpl foreignProps)
                   -> (MantineComponent props -> MantineComponent props)
                   -> JSX
mkTrivialComponent cmpt = mkComponentWithDefault cmpt defaultMantineComponent_

type Polymorphic rest =
  ( component        :: Optional String
  , polymorphicProps :: Object Foreign
  | rest
  )

type PolymorphicImpl rest =
  ( component        :: OptionalImpl String
  , polymorphicProps :: Object Foreign
  | rest
  )

type Responsive value =
  { base :: value
  , xs   :: Optional value
  , sm   :: Optional value
  , md   :: Optional value
  , lg   :: Optional value
  , xl   :: Optional value
  }

type ResponsiveImpl value =
  { base :: value
  , xs   :: OptionalImpl value
  , sm   :: OptionalImpl value
  , md   :: OptionalImpl value
  , lg   :: OptionalImpl value
  , xl   :: OptionalImpl value
  }

data FixedOrResponsive value
  = Fixed value
  | Responsive (Responsive value)

type FixedOrResponsiveImpl value = value |+| ResponsiveImpl value

instance ToFFI ps js => ToFFI (FixedOrResponsive ps) (FixedOrResponsiveImpl js) where
  toNative = case _ of
    Fixed      f -> asOneOf (toNative f)
    Responsive r -> asOneOf (toNative r)

type PopoverMiddlewares =
  { shift  :: Boolean
  , flip   :: Boolean
  , inline :: Optional Boolean
  }

type PopoverMiddlewaresImpl =
  { shift  :: Boolean
  , flip   :: Boolean
  , inline :: OptionalImpl Boolean
  }

type Controlled  value = Controlled_ value ()
type Controlled_ value rest =
  ( defaultValue :: Optional     value
  , onChange     :: ValueHandler value
  , value        :: Optional     value
  | rest
  )

type ControlledImpl  value = ControlledImpl_ value ()
type ControlledImpl_ value rest =
  ( defaultValue :: OptionalImpl     value
  , onChange     :: ValueHandlerImpl value
  , value        :: OptionalImpl     value
  | rest
  )

data Breakpoint
  = BreakpointExtraSmall
  | BreakpointSmall
  | BreakpointMedium
  | BreakpointLarge
  | BreakpointExtraLarge

type BreakpointImpl = String

instance ToFFI Breakpoint BreakpointImpl where
  toNative = case _ of
    BreakpointExtraSmall -> "xs"
    BreakpointSmall      -> "sm"
    BreakpointMedium     -> "md"
    BreakpointLarge      -> "lg"
    BreakpointExtraLarge -> "xl"
