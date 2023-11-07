module Mantine.Core.Inputs.Chip
  ( chip
  , ChipProps
  , ChipType(..)
  , ChipVariant(..)

  , chipGroup
  , multipleChipGroup
  , ChipGroupProps
  ) where

import Prelude (class Show)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Mantine.Core.Prelude
import Web.HTML.HTMLDivElement (HTMLDivElement)

chip :: (ChipProps -> ChipProps) -> JSX
chip = mkComponentWithDefault chipComponent defaultChipProps

foreign import chipComponent :: ReactComponent ChipPropsImpl

type ChipProps =
  ThemingProps
    ( checked        :: Maybe Boolean
    , children       :: Array JSX
    , color          :: Maybe MantineColor
    , defaultChecked :: Maybe Boolean
    , icon           :: Maybe JSX
    , id             :: Maybe String
    , onChange       :: CheckerHandler
    , radius         :: Radius
    , rootRef        :: Maybe (Ref HTMLDivElement)
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
    , icon           :: Nullable JSX
    , id             :: Nullable String
    , onChange       :: EventHandler
    , radius         :: String
    , rootRef        :: Nullable (Ref HTMLDivElement)
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
    ( children     :: Array JSX
    , defaultValue :: Maybe value
    , onChange     :: ValueHandler value
    , value        :: Maybe value
    )

type ChipGroupPropsImpl value =
  ThemingPropsImpl
    ( children     :: Array JSX
    , defaultValue :: Nullable value
    , onChange     :: EffectFn1 value Unit
    , value        :: Nullable value
    )
