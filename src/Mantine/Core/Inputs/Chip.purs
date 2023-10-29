module Mantine.Core.Inputs.Chip
  ( chip
  , ChipProps
  , ChipType(..)
  , ChipVariant(..)

  , chipGroup
  , multipleChipGroup
  , ChipGroupProps
  , ChipGroupPosition(..)
  ) where

import Prelude (class Show)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Mantine.Core.Prelude

chip :: (ChipProps -> ChipProps) -> JSX
chip = mkComponentWithDefault chipComponent defaultChipProps

foreign import chipComponent :: ReactComponent ChipPropsImpl

type ChipProps =
  ThemingProps
    ( checked        :: Maybe Boolean
    , children       :: Array JSX
    , color          :: Maybe MantineColor
    , defaultChecked :: Maybe Boolean
    , id             :: Maybe String
    , onChange       :: CheckerHandler
    , radius         :: Radius
    , size           :: MantineSize
    , type           :: Maybe ChipType
    , value          :: Maybe String
    , variant        :: ChipVariant
    )

defaultChipProps :: ChipProps
defaultChipProps =
  defaultThemingProps
    { size:   Small
    , radius: RadiusPreset ExtraLarge
    }

type ChipPropsImpl =
  ThemingPropsImpl
    ( checked        :: Nullable Boolean
    , children       :: Array JSX
    , color          :: Nullable String
    , defaultChecked :: Nullable Boolean
    , id             :: Nullable String
    , onChange       :: EventHandler
    , radius         :: String
    , size           :: String
    , type           :: Nullable String
    , value          :: Nullable String
    , variant        :: String
    )

data ChipType
  = ChipTypeCheckbox
  | ChipTypeRadio

instance ToFFI ChipType String where
  toNative = case _ of
    ChipTypeCheckbox -> "checkbox"
    ChipTypeRadio    -> "radio"

data LoaderPosition
  = LoaderPositionLeft
  | LoaderPositionRight
  | LoaderPositionCenter

data ChipVariant
  = ChipVariantFilled
  | ChipVariantOutline
  | ChipVariantLight

instance DefaultValue ChipVariant where defaultValue = ChipVariantOutline

instance ToFFI ChipVariant String where
  toNative = case _ of
    ChipVariantOutline -> "outline"
    ChipVariantFilled  -> "filled"
    ChipVariantLight   -> "light"

derive instance genericChipVariant :: Generic ChipVariant _
instance showChipVariant :: Show ChipVariant where show = genericShow

chipGroup :: (SingleChipGroupProps -> SingleChipGroupProps) -> JSX
chipGroup = mkTrivialComponent chipGroupComponent

foreign import chipGroupComponent :: ReactComponent (ChipGroupPropsImpl String)

type SingleChipGroupProps = ChipGroupProps String

multipleChipGroup :: (MultipleChipGroupProps -> MultipleChipGroupProps) -> JSX
multipleChipGroup = mkTrivialComponent multipleChipGroupComponent

foreign import multipleChipGroupComponent :: ReactComponent (ChipGroupPropsImpl (Array String))

type MultipleChipGroupProps = ChipGroupProps (Array String)

type ChipGroupProps value =
  ThemingProps
    ( align        :: Maybe AlignItems
    , children     :: Array JSX
    , defaultValue :: Maybe value
    , grow         :: Boolean
    , noWrap       :: Boolean
    , onChange     :: ValueHandler value
    , position     :: Maybe ChipGroupPosition
    , spacing      :: Maybe MantineNumberSize
    , value        :: Maybe value
    )

data ChipGroupPosition
  = ChipGroupPositionLeft
  | ChipGroupPositionRight
  | ChipGroupPositionCenter
  | ChipGroupPositionApart

instance ToFFI ChipGroupPosition String where
 toNative = case _ of
  ChipGroupPositionLeft   -> "left"
  ChipGroupPositionRight  -> "right"
  ChipGroupPositionCenter -> "center"
  ChipGroupPositionApart  -> "apart"

type ChipGroupPropsImpl value =
  ThemingPropsImpl
    ( align        :: Nullable String
    , children     :: Array JSX
    , defaultValue :: Nullable value
    , grow         :: Boolean
    , noWrap       :: Boolean
    , onChange     :: EffectFn1 value Unit
    , position     :: Nullable String
    , spacing      :: Nullable MantineNumberSizeImpl
    , value        :: Nullable value
    )
