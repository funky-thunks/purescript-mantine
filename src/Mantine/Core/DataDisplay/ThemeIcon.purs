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
  ThemingProps
    ( children :: Array JSX
    , color    :: Maybe MantineColor
    , radius   :: MantineNumberSize
    , size     :: MantineNumberSize
    , variant  :: ThemeIconVariant
    )

defaultThemeIconProps :: ThemeIconProps
defaultThemeIconProps =
  defaultThemingProps
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

instance ToFFI ThemeIconVariant (Nullable String) where
  toNative = toNative <<< case _ of
    ThemeIconFilled     -> Nothing
    ThemeIconLight      -> Just "light"
    ThemeIconOutline    -> Just "outline"
    ThemeIconDefault    -> Just "default"
    ThemeIconWhite      -> Just "white"
    ThemeIconGradient _ -> Just "gradient"

type ThemeIconPropsImpl =
  ThemingPropsImpl
    ( children :: Array JSX
    , color    :: Nullable String
    , gradient :: Nullable MantineGradientImpl
    , radius   :: MantineNumberSizeImpl
    , size     :: MantineNumberSizeImpl
    , variant  :: Nullable String
    )

themeIconToImpl :: ThemeIconProps -> ThemeIconPropsImpl
themeIconToImpl props =
  let gradient = case props.variant of
        ThemeIconGradient g -> pure g
        _                   -> Nothing
   in toNative ({ gradient } `union` props)
