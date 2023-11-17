module Mantine.Core.DataDisplay.ThemeIcon
  ( themeIcon
  , Props_ThemeIcon
  , Props_ThemeIconImpl
  , ThemeIconVariant(..)
  , ThemeIconVariantImpl
  ) where

import Mantine.Core.Prelude

themeIcon
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_ThemeIcon
  => Union attrsImpl attrsImpl_ Props_ThemeIconImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
themeIcon = element (unsafeCoerce themeIconComponent) <<< toNative

foreign import themeIconComponent :: ReactComponent (Record Props_ThemeIconImpl)

type Props_ThemeIcon =
  Props_Common
    ( children :: Array JSX
    , color    :: MantineColor
    , gradient :: MantineGradient
    , radius   :: MantineNumberSize
    , size     :: MantineNumberSize
    , variant  :: ThemeIconVariant
    )

data ThemeIconVariant
  = ThemeIconFilled
  | ThemeIconLight
  | ThemeIconOutline
  | ThemeIconDefault
  | ThemeIconWhite
  | ThemeIconGradient

type ThemeIconVariantImpl = OptionalImpl String

instance ToFFI ThemeIconVariant ThemeIconVariantImpl where
  toNative = toNative <<< Optional <<< case _ of
    ThemeIconFilled   -> Nothing
    ThemeIconLight    -> Just "light"
    ThemeIconOutline  -> Just "outline"
    ThemeIconDefault  -> Just "default"
    ThemeIconWhite    -> Just "white"
    ThemeIconGradient -> Just "gradient"

type Props_ThemeIconImpl =
  Props_CommonImpl
    ( children :: Array JSX
    , color    :: MantineColorImpl
    , gradient :: MantineGradientImpl
    , radius   :: MantineNumberSizeImpl
    , size     :: MantineNumberSizeImpl
    , variant  :: ThemeIconVariantImpl
    )
