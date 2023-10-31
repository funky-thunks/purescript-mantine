module Mantine.Core.DataDisplay.Badge
  ( badge
  , badge_
  , BadgeProps
  , BadgeVariant(..)
  ) where

import Mantine.Core.Prelude

badge :: (BadgeProps -> BadgeProps) -> JSX
badge = mkComponent badgeComponent badgeToImpl defaultBadgeProps

badge_ :: Array JSX -> JSX
badge_ children = badge _ { children = children }

foreign import badgeComponent :: ReactComponent BadgePropsImpl

type BadgeProps =
  MantineComponent
    ( children     :: Array JSX
    , color        :: Maybe MantineColor
    , fullWidth    :: Boolean
    , leftSection  :: Maybe JSX
    , radius       :: MantineNumberSize
    , rightSection :: Maybe JSX
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
  | BadgeVariantGradient MantineGradient

instance DefaultValue BadgeVariant where
  defaultValue = BadgeVariantFilled

type BadgeVariantImpl = String

instance ToFFI BadgeVariant BadgeVariantImpl where
  toNative = case _ of
    BadgeVariantFilled      -> "filled"
    BadgeVariantLight       -> "light"
    BadgeVariantOutline     -> "outline"
    BadgeVariantDot         -> "dot"
    BadgeVariantTransparent -> "transparent"
    BadgeVariantDefault     -> "default"
    BadgeVariantGradient _  -> "gradient"

defaultBadgeProps :: BadgeProps
defaultBadgeProps =
  defaultMantineComponent
    { size:   Medium
    , radius: Preset ExtraLarge
    }

type BadgePropsImpl =
  MantineComponentImpl
    ( children     :: Array JSX
    , color        :: Nullable MantineColorImpl
    , fullWidth    :: Boolean
    , gradient     :: Nullable MantineGradientImpl
    , leftSection  :: Nullable JSX
    , radius       :: MantineNumberSizeImpl
    , rightSection :: Nullable JSX
    , size         :: MantineSizeImpl
    , variant      :: BadgeVariantImpl
    )

badgeToImpl :: BadgeProps -> BadgePropsImpl
badgeToImpl props = toNative (props `union` { gradient: getGradient props.variant })

getGradient :: BadgeVariant -> Maybe MantineGradient
getGradient = case _ of
  BadgeVariantGradient g -> pure g
  _                      -> Nothing
