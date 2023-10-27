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
  ThemingProps
    ( children   :: Array JSX
    , radius     :: MantineNumberSize
 -- , shadow     :: MantineShadow -- TODO
    , withBorder :: Boolean
    )

defaultCardProps :: CardProps
defaultCardProps = defaultThemingProps { radius: Preset Small }

type CardPropsImpl =
  ThemingPropsImpl
    ( children   :: Array JSX
    , radius     :: MantineNumberSizeImpl
 -- , shadow     :: MantineShadowImpl -- TODO
    , withBorder :: Boolean
    )

cardSection :: (CardSectionProps -> CardSectionProps) -> JSX
cardSection = mkComponent cardSectionComponent toNative defaultThemingProps_

foreign import cardSectionComponent :: ReactComponent CardSectionPropsImpl

type CardSectionProps = ThemingProps (Polymorphic (children :: Array  JSX))

type CardSectionPropsImpl = ThemingPropsImpl (PolymorphicImpl (children :: Array JSX))
