module Mantine.Core.Inputs.Chip
  ( chip
  , Props_Chip
  , Props_ChipImpl
  , ChipType(..)
  , ChipTypeImpl
  , ChipVariant(..)
  , ChipVariantImpl

  , chipGroup
  , multipleChipGroup
  , Props_ChipGroup
  , Props_ChipGroupImpl
  ) where

import Prelude (class Show)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Mantine.Core.Inputs.Checkables (Props_CheckableComponent, Props_CheckableComponentImpl)
import Mantine.Core.Prelude

chip
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Chip
  => Union attrsImpl attrsImpl_ Props_ChipImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
chip = element (unsafeCoerce chipComponent) <<< toNative

foreign import chipComponent :: ReactComponent (Record Props_ChipImpl)

type Props_Chip =
  Props_CheckableComponent
    ( children :: Array JSX
    , icon     :: JSX
    , type     :: ChipType
    , variant  :: ChipVariant
    )

type Props_ChipImpl =
  Props_CheckableComponentImpl
    ( children :: Array JSX
    , icon     :: JSX
    , type     :: ChipTypeImpl
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

type ChipVariantImpl = String

instance ToFFI ChipVariant ChipVariantImpl where
  toNative = case _ of
    ChipVariantOutline -> "outline"
    ChipVariantFilled  -> "filled"
    ChipVariantLight   -> "light"

derive instance genericChipVariant :: Generic ChipVariant _
instance showChipVariant :: Show ChipVariant where show = genericShow

chipGroup
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_SingleChipGroup
  => Union attrsImpl attrsImpl_ Props_SingleChipGroupImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
chipGroup = element (unsafeCoerce chipGroupComponent) <<< toNative

foreign import chipGroupComponent :: ReactComponent (Record Props_SingleChipGroupImpl)

type Props_SingleChipGroup     = Props_ChipGroup     String
type Props_SingleChipGroupImpl = Props_ChipGroupImpl String

multipleChipGroup
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_MultipleChipGroup
  => Union attrsImpl attrsImpl_ Props_MultipleChipGroupImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
multipleChipGroup = element (unsafeCoerce multipleChipGroupComponent) <<< toNative

foreign import multipleChipGroupComponent :: ReactComponent (Record Props_MultipleChipGroupImpl)

type Props_MultipleChipGroup     = Props_ChipGroup     (Array String)
type Props_MultipleChipGroupImpl = Props_ChipGroupImpl (Array String)

type Props_ChipGroup value =
  Props_Common
    ( children :: Array JSX
    | Controlled value
    )

type Props_ChipGroupImpl value =
  Props_CommonImpl
    ( children :: Array JSX
    | ControlledImpl value
    )
