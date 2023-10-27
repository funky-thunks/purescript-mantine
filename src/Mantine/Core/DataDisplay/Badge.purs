module Mantine.Core.DataDisplay.Badge
  ( badge
  , badge_
  , BadgeProps
  , BadgeVariant(..)
  ) where

import Prelude ((=<<), pure)
import Mantine.Core.Prelude

badge :: (BadgeProps -> BadgeProps) -> JSX
badge = mkComponent badgeComponent badgeToImpl defaultBadgeProps

badge_ :: Array JSX -> JSX
badge_ children = badge _ { children = children }

foreign import badgeComponent :: ReactComponent BadgePropsImpl

type BadgeProps =
  ThemingProps
    ( children     :: Array JSX
    , fullWidth    :: Boolean
    , color        :: Maybe MantineColor
    , size         :: MantineSize
    , radius       :: MantineNumberSize
    , variant      :: BadgeVariant
    , leftSection  :: Maybe JSX
    , rightSection :: Maybe JSX
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
    , fullWidth    :: Boolean
    , color        :: Nullable String
    , size         :: String
    , radius       :: MantineNumberSizeImpl
    , variant      :: String
    , gradient     :: Nullable MantineGradientImpl
    , leftSection  :: Nullable JSX
    , rightSection :: Nullable JSX
    )

badgeToImpl :: BadgeProps -> BadgePropsImpl
badgeToImpl props = toNative (props `union` { gradient: getGradient props.variant })

getGradient :: BadgeVariant -> Maybe MantineGradient
getGradient = case _ of
  BadgeVariantGradient g -> pure g
  _                      -> Nothing
