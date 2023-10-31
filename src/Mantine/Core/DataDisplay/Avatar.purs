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

type AvatarProps =
  ThemingProps
    ( alt      :: Maybe String
    , children :: Array JSX
    , color    :: Maybe MantineColor
    , radius   :: MantineNumberSize
    , size     :: MantineNumberSize
    , src      :: Maybe String
    , variant  :: AvatarVariant
    )

data AvatarVariant
  = AvatarVariantOutline
  | AvatarVariantLight
  | AvatarVariantFilled
  | AvatarVariantGradient MantineGradient

instance DefaultValue AvatarVariant where defaultValue = AvatarVariantLight

instance ToFFI AvatarVariant String where
  toNative = case _ of
    AvatarVariantOutline    -> "outline"
    AvatarVariantLight      -> "light"
    AvatarVariantFilled     -> "filled"
    AvatarVariantGradient _ -> "gradient"

defaultAvatarProps :: AvatarProps
defaultAvatarProps =
  defaultThemingProps
    { size:   Preset Medium
    , radius: Preset Small
    }

type AvatarPropsImpl =
  ThemingPropsImpl
    ( alt      :: Nullable String
    , children :: Array JSX
    , color    :: Nullable String
    , gradient :: Nullable MantineGradientImpl
    , radius   :: MantineNumberSizeImpl
    , size     :: MantineNumberSizeImpl
    , src      :: Nullable String
    , variant  :: String
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
  ThemingProps
    ( children :: Array JSX
    , spacing  :: Maybe MantineNumberSize
    )

type AvatarGroupPropsImpl =
  ThemingPropsImpl
    ( children :: Array JSX
    , spacing  :: Nullable MantineNumberSizeImpl
    )
