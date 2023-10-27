module Mantine.Core.Inputs.Chip
  ( chip
  , ChipProps
  , ChipType(..)
  , ChipVariant(..)

  , chipGroup
  , ChipGroupSingle(..)
  , multipleChipGroup
  , ChipGroupMultiple(..)
  , ChipGroupProps
  , ChipGroupPosition(..)
  ) where

import Prelude (class Show)
import Data.Array (last)
import Data.Generic.Rep (class Generic)
import Data.Maybe (maybe)
import Data.Show.Generic (genericShow)
import Mantine.Core.Prelude

chip :: (ChipProps -> ChipProps) -> JSX
chip = mkComponentWithDefault chipComponent defaultChipProps

foreign import chipComponent :: ReactComponent ChipPropsImpl

type ChipProps =
  ThemingProps
    ( children       :: Array JSX
    , checked        :: Maybe Boolean
    , color          :: Maybe MantineColor
    , defaultChecked :: Maybe Boolean
    , id             :: Maybe String
    , onChange       :: CheckerHandler
    , radius         :: Radius
    , size           :: MantineSize
    , type           :: Maybe ChipType
    , value          :: Maybe String
    , variant        :: Maybe ChipVariant
    )

defaultChipProps :: ChipProps
defaultChipProps =
  defaultThemingProps
    { size:   Small
    , radius: RadiusPreset ExtraLarge
    }

type ChipPropsImpl =
  ThemingPropsImpl
    ( children       :: Array JSX
    , onChange       :: EventHandler
    , id             :: Nullable String
    , value          :: Nullable String
    , checked        :: Nullable Boolean
    , defaultChecked :: Nullable Boolean
    , color          :: Nullable String
    , radius         :: String
    , size           :: String
    , type           :: Nullable String
    , variant        :: Nullable String
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

instance ToFFI ChipVariant String where
  toNative = case _ of
    ChipVariantOutline -> "outline"
    ChipVariantFilled  -> "filled"

derive instance genericChipVariant :: Generic ChipVariant _
instance showChipVariant :: Show ChipVariant where show = genericShow

chipGroup :: (SingleChipGroupProps -> SingleChipGroupProps) -> JSX
chipGroup = mkComponent chipGroupComponent toNative defaultThemingProps_

foreign import chipGroupComponent :: ReactComponent SingleChipGroupPropsImpl

type SingleChipGroupProps     = ChipGroupProps ChipGroupSingle
type SingleChipGroupPropsImpl = ChipGroupPropsImpl

multipleChipGroup :: (MultipleChipGroupProps -> MultipleChipGroupProps) -> JSX
multipleChipGroup = mkComponent multipleChipGroupComponent toNative defaultThemingProps_

foreign import multipleChipGroupComponent :: ReactComponent MultipleChipGroupPropsImpl

type MultipleChipGroupProps     = ChipGroupProps ChipGroupMultiple
type MultipleChipGroupPropsImpl = ChipGroupPropsImpl

type ChipGroupProps value =
  ThemingProps
    ( align        :: Maybe AlignItems
    , defaultValue :: Maybe value
    , grow         :: Boolean
    , noWrap       :: Boolean
    , onChange     :: ValueHandler value
    , position     :: Maybe ChipGroupPosition
    , spacing      :: Maybe MantineNumberSize
    , value        :: Maybe value
    , children     :: Array JSX
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

newtype ChipGroupSingle = ChipGroupSingle (Maybe String)

instance ToFFI ChipGroupSingle (Array String) where
  toNative (ChipGroupSingle v) = maybe [] pure v

instance FromFFI (Array String) ChipGroupSingle where
  fromNative = ChipGroupSingle <<< last

newtype ChipGroupMultiple = ChipGroupMultiple (Array String)

instance ToFFI ChipGroupMultiple (Array String) where
  toNative (ChipGroupMultiple vs) = vs

instance FromFFI (Array String) ChipGroupMultiple where
  fromNative = ChipGroupMultiple

type ChipGroupPropsImpl =
  ThemingPropsImpl
    ( align        :: Nullable String
    , defaultValue :: Nullable (Array String)
    , grow         :: Boolean
    , noWrap       :: Boolean
    , onChange     :: EffectFn1 (Array String) Unit
    , position     :: Nullable String
    , spacing      :: Nullable MantineNumberSizeImpl
    , value        :: Nullable (Array String)
    , children     :: Array JSX
    )
