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
    , color        :: Optional MantineColor
    , fullWidth    :: Boolean
    , leftSection  :: Optional JSX
    , radius       :: MantineNumberSize
    , rightSection :: Optional JSX
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
    , color        :: OptionalImpl MantineColorImpl
    , fullWidth    :: Boolean
    , gradient     :: OptionalImpl MantineGradientImpl
    , leftSection  :: OptionalImpl JSX
    , radius       :: MantineNumberSizeImpl
    , rightSection :: OptionalImpl JSX
    , size         :: MantineSizeImpl
    , variant      :: BadgeVariantImpl
    )

badgeToImpl :: BadgeProps -> BadgePropsImpl
badgeToImpl props = toNative (props `union` { gradient: getGradient props.variant })

getGradient :: BadgeVariant -> Optional MantineGradient
getGradient = Optional <<< case _ of
  BadgeVariantGradient g -> pure g
  _                      -> Nothing
