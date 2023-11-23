module Mantine.Core.DataDisplay.Badge
  ( badge
  , badge_
  , Props_Badge
  , Props_BadgeImpl
  , BadgeVariant(..)
  , BadgeVariantImpl
  ) where

import Mantine.Core.Prelude

badge
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Badge
  => Union attrsImpl attrsImpl_ Props_BadgeImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
badge = element (unsafeCoerce badgeComponent) <<< toNative

badge_ :: Array JSX -> JSX
badge_ children = badge { children }

foreign import badgeComponent :: ReactComponent (Record Props_BadgeImpl)

type Props_Badge =
  Props_Common
    ( children     :: Array JSX
    , color        :: MantineColor
    , fullWidth    :: Boolean
    , gradient     :: MantineGradient
    , leftSection  :: JSX
    , radius       :: MantineNumberSize
    , rightSection :: JSX
    , size         :: MantineSize
    , variant      :: BadgeVariant
    )

data BadgeVariant
  = BadgeVariantFilled
  | BadgeVariantLight
  | BadgeVariantOutline
  | BadgeVariantDot
  | BadgeVariantTransparent
  | BadgeVariantDefault
  | BadgeVariantGradient

type BadgeVariantImpl = String

instance ToFFI BadgeVariant BadgeVariantImpl where
  toNative = case _ of
    BadgeVariantFilled      -> "filled"
    BadgeVariantLight       -> "light"
    BadgeVariantOutline     -> "outline"
    BadgeVariantDot         -> "dot"
    BadgeVariantTransparent -> "transparent"
    BadgeVariantDefault     -> "default"
    BadgeVariantGradient    -> "gradient"

type Props_BadgeImpl =
  Props_CommonImpl
    ( children     :: Array JSX
    , color        :: MantineColorImpl
    , fullWidth    :: Boolean
    , gradient     :: MantineGradientImpl
    , leftSection  :: JSX
    , radius       :: MantineNumberSizeImpl
    , rightSection :: JSX
    , size         :: MantineSizeImpl
    , variant      :: BadgeVariantImpl
    )
