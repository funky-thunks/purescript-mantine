module Mantine.Core.DataDisplay.ThemeIcon
  ( themeIcon
  , ThemeIconProps
  , ThemeIconVariant(..)
  ) where

import Mantine.Core.Prelude

themeIcon :: (ThemeIconProps -> ThemeIconProps) -> JSX
themeIcon = mkComponent themeIconComponent themeIconToImpl defaultThemeIconProps

foreign import themeIconComponent :: ReactComponent ThemeIconPropsImpl

type ThemeIconProps =
  MantineComponent
    ( children :: Array JSX
    , color    :: Optional MantineColor
    , radius   :: MantineNumberSize
    , size     :: MantineNumberSize
    , variant  :: ThemeIconVariant
    )

defaultThemeIconProps :: ThemeIconProps
defaultThemeIconProps =
  defaultMantineComponent
    { size:   Preset Medium
    , radius: Preset Small
    }

data ThemeIconVariant
  = ThemeIconFilled
  | ThemeIconLight
  | ThemeIconOutline
  | ThemeIconDefault
  | ThemeIconWhite
  | ThemeIconGradient MantineGradient

instance DefaultValue ThemeIconVariant where
  defaultValue = ThemeIconFilled

type ThemeIconVariantImpl = OptionalImpl String

instance ToFFI ThemeIconVariant ThemeIconVariantImpl where
  toNative = toNative <<< Optional <<< case _ of
    ThemeIconFilled     -> Nothing
    ThemeIconLight      -> Just "light"
    ThemeIconOutline    -> Just "outline"
    ThemeIconDefault    -> Just "default"
    ThemeIconWhite      -> Just "white"
    ThemeIconGradient _ -> Just "gradient"

type ThemeIconPropsImpl =
  MantineComponentImpl
    ( children :: Array JSX
    , color    :: OptionalImpl MantineColorImpl
    , gradient :: OptionalImpl MantineGradientImpl
    , radius   :: MantineNumberSizeImpl
    , size     :: MantineNumberSizeImpl
    , variant  :: ThemeIconVariantImpl
    )

themeIconToImpl :: ThemeIconProps -> ThemeIconPropsImpl
themeIconToImpl props =
  let gradient = Optional $ case props.variant of
        ThemeIconGradient g -> pure g
        _                   -> Nothing
   in toNative ({ gradient } `union` props)
