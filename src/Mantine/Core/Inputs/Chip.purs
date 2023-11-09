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
import Mantine.Core.Inputs.Checkables (CheckableComponent, CheckableComponentImpl)
import Mantine.Core.Prelude

chip :: (ChipProps -> ChipProps) -> JSX
chip = mkTrivialComponent chipComponent

foreign import chipComponent :: ReactComponent ChipPropsImpl

type ChipProps =
  CheckableComponent
    ( children :: Array JSX
    , icon     :: Maybe JSX
    , type     :: Maybe ChipType
    , variant  :: ChipVariant
    )

type ChipPropsImpl =
  CheckableComponentImpl
    ( children :: Array JSX
    , icon     :: Nullable JSX
    , type     :: Nullable ChipTypeImpl
    , variant  :: ChipVariantImpl
    )

data ChipType
  = ChipTypeCheckbox
  | ChipTypeRadio

type ChipTypeImpl = String

instance ToFFI ChipType ChipTypeImpl where
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

type ChipVariantImpl = String

instance ToFFI ChipVariant ChipVariantImpl where
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
  MantineComponent
    ( children :: Array JSX
    | Controlled value
    )

type ChipGroupPropsImpl value =
  MantineComponentImpl
    ( children :: Array JSX
    | ControlledImpl value
    )
