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
  = ThemeIconOutline
  | ThemeIconLight
  | ThemeIconDefault
  | ThemeIconFilled
  | ThemeIconGradient MantineGradient

instance DefaultValue ThemeIconVariant where defaultValue = ThemeIconDefault

instance ToFFI ThemeIconVariant String where
  toNative = case _ of
    ThemeIconOutline    -> "outline"
    ThemeIconLight      -> "light"
    ThemeIconDefault    -> "default"
    ThemeIconFilled     -> "filled"
    ThemeIconGradient _ -> "gradient"

type ThemeIconPropsImpl =
  ThemingPropsImpl
    ( children :: Array JSX
    , color    :: Nullable String
    , gradient :: Nullable MantineGradientImpl
    , radius   :: MantineNumberSizeImpl
    , size     :: MantineNumberSizeImpl
    , variant  :: String
    )

themeIconToImpl :: ThemeIconProps -> ThemeIconPropsImpl
themeIconToImpl props =
  let gradient = case props.variant of
        ThemeIconGradient g -> pure g
        _                   -> Nothing
   in toNative ({ gradient } `union` props)
