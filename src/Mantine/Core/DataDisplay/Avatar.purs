module Mantine.Core.DataDisplay.Avatar
  ( avatar
  , AvatarProps
  , AvatarVariant(..)

  , avatarGroup
  , AvatarGroupProps
  ) where

import Mantine.Core.Prelude

avatar :: (AvatarProps -> AvatarProps) -> JSX
avatar = mkComponent avatarComponent avatarToImpl defaultAvatarProps

foreign import avatarComponent :: ReactComponent AvatarPropsImpl

-- Not supported properties
--   { imageProps :: React.ComponentPropsWithoutRef<"img">
--   }

type AvatarProps =
  MantineComponent
    ( alt      :: Optional String
    , children :: Array JSX
    , color    :: Optional MantineColor
    , radius   :: MantineNumberSize
    , size     :: MantineNumberSize
    , src      :: Optional String
    , variant  :: AvatarVariant
    )

data AvatarVariant
  = AvatarVariantFilled
  | AvatarVariantLight
  | AvatarVariantOutline
  | AvatarVariantTransparent
  | AvatarVariantWhite
  | AvatarVariantDefault
  | AvatarVariantGradient MantineGradient

instance DefaultValue AvatarVariant where defaultValue = AvatarVariantLight

type AvatarVariantImpl = String

instance ToFFI AvatarVariant AvatarVariantImpl where
  toNative = case _ of
    AvatarVariantFilled      -> "filled"
    AvatarVariantLight       -> "light"
    AvatarVariantOutline     -> "outline"
    AvatarVariantTransparent -> "transparent"
    AvatarVariantWhite       -> "white"
    AvatarVariantDefault     -> "default"
    AvatarVariantGradient _  -> "gradient"

defaultAvatarProps :: AvatarProps
defaultAvatarProps =
  defaultMantineComponent
    { size:   Preset Medium
    , radius: Preset Small
    }

type AvatarPropsImpl =
  MantineComponentImpl
    ( alt      :: OptionalImpl String
    , children :: Array JSX
    , color    :: OptionalImpl MantineColorImpl
    , gradient :: OptionalImpl MantineGradientImpl
    , radius   :: MantineNumberSizeImpl
    , size     :: MantineNumberSizeImpl
    , src      :: OptionalImpl String
    , variant  :: AvatarVariantImpl
    )

avatarToImpl :: AvatarProps -> AvatarPropsImpl
avatarToImpl props = toNative (props `union` { gradient: getGradient props.variant })

getGradient :: AvatarVariant -> Optional MantineGradient
getGradient = Optional <<< case _ of
  AvatarVariantGradient g -> pure g
  _                       -> Nothing

avatarGroup :: (AvatarGroupProps -> AvatarGroupProps) -> JSX
avatarGroup = mkTrivialComponent avatarGroupComponent

foreign import avatarGroupComponent :: ReactComponent AvatarGroupPropsImpl

type AvatarGroupProps =
  MantineComponent
    ( children :: Array JSX
    , spacing  :: Optional MantineNumberSize
    )

type AvatarGroupPropsImpl =
  MantineComponentImpl
    ( children :: Array JSX
    , spacing  :: OptionalImpl MantineNumberSizeImpl
    )
