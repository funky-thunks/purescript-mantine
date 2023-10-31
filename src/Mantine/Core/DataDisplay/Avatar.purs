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
    ( alt      :: Maybe String
    , children :: Array JSX
    , color    :: Maybe MantineColor
    , radius   :: MantineNumberSize
    , size     :: MantineNumberSize
    , src      :: Maybe String
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
    ( alt      :: Nullable String
    , children :: Array JSX
    , color    :: Nullable MantineColorImpl
    , gradient :: Nullable MantineGradientImpl
    , radius   :: MantineNumberSizeImpl
    , size     :: MantineNumberSizeImpl
    , src      :: Nullable String
    , variant  :: AvatarVariantImpl
    )

avatarToImpl :: AvatarProps -> AvatarPropsImpl
avatarToImpl props = toNative (props `union` { gradient: getGradient props.variant })

getGradient :: AvatarVariant -> Maybe MantineGradient
getGradient = case _ of
  AvatarVariantGradient g -> pure g
  _                       -> Nothing

avatarGroup :: (AvatarGroupProps -> AvatarGroupProps) -> JSX
avatarGroup = mkTrivialComponent avatarGroupComponent

foreign import avatarGroupComponent :: ReactComponent AvatarGroupPropsImpl

type AvatarGroupProps =
  MantineComponent
    ( children :: Array JSX
    , spacing  :: Maybe MantineNumberSize
    )

type AvatarGroupPropsImpl =
  MantineComponentImpl
    ( children :: Array JSX
    , spacing  :: Nullable MantineNumberSizeImpl
    )
