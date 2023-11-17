module Mantine.Core.DataDisplay.Card
  ( card
  , CardProps

  , cardSection
  , CardSectionProps
  ) where

import Mantine.Core.Prelude

card :: (CardProps -> CardProps) -> JSX
card = mkComponentWithDefault cardComponent defaultCardProps

foreign import cardComponent :: ReactComponent CardPropsImpl

type CardProps =
  MantineComponent
    ( children   :: Array JSX
    , padding    :: Optional MantineNumberSize
    , radius     :: MantineNumberSize
    , shadow     :: Optional MantineShadow
    , withBorder :: Boolean
    )

defaultCardProps :: CardProps
defaultCardProps = defaultMantineComponent { radius: Preset Small }

type CardPropsImpl =
  MantineComponentImpl
    ( children   :: Array JSX
    , padding    :: OptionalImpl MantineNumberSizeImpl
    , radius     :: MantineNumberSizeImpl
    , shadow     :: OptionalImpl MantineShadowImpl
    , withBorder :: Boolean
    )

cardSection :: (CardSectionProps -> CardSectionProps) -> JSX
cardSection = mkComponent cardSectionComponent toNative defaultMantineComponent_

foreign import cardSectionComponent :: ReactComponent CardSectionPropsImpl

type CardSectionProps =
  MantineComponent (
    Polymorphic
      ( children       :: Array JSX
      , inheritPadding :: Boolean
      , withBorder     :: Boolean
      )
  )

type CardSectionPropsImpl =
  MantineComponentImpl (
    PolymorphicImpl
      ( children       :: Array JSX
      , inheritPadding :: Boolean
      , withBorder     :: Boolean
      )
  )
