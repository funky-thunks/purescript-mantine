module Mantine.Core.DataDisplay.Card
  ( card
  , CardProps

  , cardSection
  , CardSectionProps
  ) where

import Foreign (Foreign)
import Mantine.Core.Prelude

card :: (CardProps -> CardProps) -> JSX
card = mkComponent cardComponent toNative defaultCardProps

foreign import cardComponent :: ReactComponent CardPropsImpl

type CardProps =
  ThemingProps
    ( children   :: Array JSX
    , radius     :: MantineNumberSize
 -- , shadow     :: MantineShadow -- TODO
    , withBorder :: Boolean
    )

defaultCardProps :: CardProps
defaultCardProps =
  defaultThemingProps
    { radius: Preset Small
    } `union` defaultValue

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

type CardSectionProps =
  ThemingProps
    ( children         :: Array  JSX
    , component        :: Maybe  String
    , polymorphicProps :: Object Foreign
    )

type CardSectionPropsImpl =
  ThemingPropsImpl
    ( children         :: Array JSX
    , component        :: Nullable String
    , polymorphicProps :: Object Foreign
    )
