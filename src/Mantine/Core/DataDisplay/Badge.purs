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
  ThemingProps
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
  = BadgeVariantOutline
  | BadgeVariantLight
  | BadgeVariantFilled
  | BadgeVariantDot
  | BadgeVariantGradient MantineGradient

instance DefaultValue BadgeVariant where defaultValue = BadgeVariantLight

instance ToFFI BadgeVariant String where
  toNative = case _ of
    BadgeVariantOutline    -> "outline"
    BadgeVariantLight      -> "light"
    BadgeVariantFilled     -> "filled"
    BadgeVariantDot        -> "dot"
    BadgeVariantGradient _ -> "gradient"

defaultBadgeProps :: BadgeProps
defaultBadgeProps =
  defaultThemingProps
    { size:   Medium
    , radius: Preset ExtraLarge
    }

type BadgePropsImpl =
  ThemingPropsImpl
    ( children     :: Array JSX
    , color        :: Nullable String
    , fullWidth    :: Boolean
    , gradient     :: Nullable MantineGradientImpl
    , leftSection  :: Nullable JSX
    , radius       :: MantineNumberSizeImpl
    , rightSection :: Nullable JSX
    , size         :: String
    , variant      :: String
    )

badgeToImpl :: BadgeProps -> BadgePropsImpl
badgeToImpl props = toNative (props `union` { gradient: getGradient props.variant })

getGradient :: BadgeVariant -> Maybe MantineGradient
getGradient = case _ of
  BadgeVariantGradient g -> pure g
  _                      -> Nothing
