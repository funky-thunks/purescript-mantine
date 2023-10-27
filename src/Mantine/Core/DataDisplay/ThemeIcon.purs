module Mantine.Core.DataDisplay.ThemeIcon
  ( themeIcon
  , ThemeIconProps
  , ThemeIconVariant(..)
  ) where

import Mantine.Core.Prelude

themeIcon :: (ThemeIconProps -> ThemeIconProps) -> JSX
themeIcon = mkComponentWithDefault themeIconComponent defaultThemeIconProps

foreign import themeIconComponent :: ReactComponent ThemeIconPropsImpl

type ThemeIconProps =
  ThemingProps
    ( children :: Array JSX
    , color    :: Maybe MantineColor
    , size     :: MantineNumberSize
    , radius   :: MantineNumberSize
    , variant  :: ThemeIconVariant
    )

defaultThemeIconProps :: ThemeIconProps
defaultThemeIconProps =
  defaultThemingProps
    { size:   Preset Medium
    , radius: Preset Small
    }

type ThemeIconPropsImpl =
  ThemingPropsImpl
    ( children :: Array JSX
    , color    :: Nullable String
    , size     :: MantineNumberSizeImpl
    , radius   :: MantineNumberSizeImpl
    , variant  :: String
    )

data ThemeIconVariant
  = ThemeIconOutline
  | ThemeIconLight
  | ThemeIconDefault
  | ThemeIconFilled
  | ThemeIconGradient

instance DefaultValue ThemeIconVariant where defaultValue = ThemeIconDefault

instance ToFFI ThemeIconVariant String where
  toNative = case _ of
    ThemeIconOutline  -> "outline"
    ThemeIconLight    -> "light"
    ThemeIconDefault  -> "default"
    ThemeIconFilled   -> "filled"
    ThemeIconGradient -> "gradient"
