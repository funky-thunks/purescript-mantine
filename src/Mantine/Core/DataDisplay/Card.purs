module Mantine.Core.DataDisplay.Card
  ( card
  , Props_Card
  , Props_CardImpl

  , cardSection
  , Props_CardSection
  , Props_CardSectionImpl
  ) where

import Mantine.Core.Prelude

card
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Card
  => Union attrsImpl attrsImpl_ Props_CardImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
card = element (unsafeCoerce cardComponent) <<< toNative

foreign import cardComponent :: ReactComponent (Record Props_CardImpl)

type Props_Card =
  Props_Common
    ( children   :: Array JSX
    , padding    :: MantineNumberSize
    , radius     :: MantineNumberSize
    , shadow     :: MantineShadow
    , withBorder :: Boolean
    )

type Props_CardImpl =
  Props_CommonImpl
    ( children   :: Array JSX
    , padding    :: MantineNumberSizeImpl
    , radius     :: MantineNumberSizeImpl
    , shadow     :: MantineShadowImpl
    , withBorder :: Boolean
    )

cardSection
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_CardSection
  => Union attrsImpl attrsImpl_ Props_CardSectionImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
cardSection = element (unsafeCoerce cardSectionComponent) <<< toNative

foreign import cardSectionComponent :: ReactComponent (Record Props_CardSectionImpl)

type Props_CardSection =
  Props_Common (
    Polymorphic
      ( children       :: Array JSX
      , inheritPadding :: Boolean
      , withBorder     :: Boolean
      )
  )

type Props_CardSectionImpl =
  Props_CommonImpl (
    PolymorphicImpl
      ( children       :: Array JSX
      , inheritPadding :: Boolean
      , withBorder     :: Boolean
      )
  )
