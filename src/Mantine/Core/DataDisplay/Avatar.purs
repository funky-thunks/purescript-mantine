module Mantine.Core.DataDisplay.Avatar
  ( avatar
  , Props_Avatar
  , Props_AvatarImpl
  , AvatarVariant(..)
  , AvatarVariantImpl

  , avatarGroup
  , Props_AvatarGroup
  , Props_AvatarGroupImpl
  ) where

import Mantine.Core.Prelude

avatar
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Avatar
  => Union attrsImpl attrsImpl_ Props_AvatarImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
avatar = element (unsafeCoerce avatarComponent) <<< toNative

foreign import avatarComponent :: ReactComponent (Record Props_AvatarImpl)

-- Not supported properties
--   { imageProps :: React.ComponentPropsWithoutRef<"img">
--   }

type Props_Avatar =
  Props_Common
    ( alt      :: String
    , children :: Array JSX
    , color    :: MantineColor
    , gradient :: MantineGradient
    , radius   :: MantineNumberSize
    , size     :: MantineNumberSize
    , src      :: String
    , variant  :: AvatarVariant
    )

data AvatarVariant
  = AvatarVariantFilled
  | AvatarVariantLight
  | AvatarVariantOutline
  | AvatarVariantTransparent
  | AvatarVariantWhite
  | AvatarVariantDefault
  | AvatarVariantGradient

type AvatarVariantImpl = String

instance ToFFI AvatarVariant AvatarVariantImpl where
  toNative = case _ of
    AvatarVariantFilled      -> "filled"
    AvatarVariantLight       -> "light"
    AvatarVariantOutline     -> "outline"
    AvatarVariantTransparent -> "transparent"
    AvatarVariantWhite       -> "white"
    AvatarVariantDefault     -> "default"
    AvatarVariantGradient    -> "gradient"

type Props_AvatarImpl =
  Props_CommonImpl
    ( alt      :: String
    , children :: Array JSX
    , color    :: MantineColorImpl
    , gradient :: MantineGradientImpl
    , radius   :: MantineNumberSizeImpl
    , size     :: MantineNumberSizeImpl
    , src      :: String
    , variant  :: AvatarVariantImpl
    )

avatarGroup
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_AvatarGroup
  => Union attrsImpl attrsImpl_ Props_AvatarGroupImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
avatarGroup = element (unsafeCoerce avatarGroupComponent) <<< toNative

foreign import avatarGroupComponent :: ReactComponent (Record Props_AvatarGroupImpl)

type Props_AvatarGroup =
  Props_Common
    ( children :: Array JSX
    , spacing  :: MantineNumberSize
    )

type Props_AvatarGroupImpl =
  Props_CommonImpl
    ( children :: Array JSX
    , spacing  :: MantineNumberSizeImpl
    )
