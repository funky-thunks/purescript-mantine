module Mantine.Core.DataDisplay.ColorSwatch
  ( colorSwatch
  , ColorSwatchProps
  ) where

import Mantine.Core.Prelude

colorSwatch :: MantineColor -> (ColorSwatchProps -> ColorSwatchProps) -> JSX
colorSwatch = mkComponent colorSwatchComponent toNative <<< defaultColorSwatch

colorSwatch_ :: MantineColor -> JSX
colorSwatch_ c = colorSwatch c identity

foreign import colorSwatchComponent :: ReactComponent ColorSwatchPropsImpl

defaultColorSwatch :: MantineColor -> ColorSwatchProps
defaultColorSwatch color =
  defaultThemingProps { color } `union` defaultValue

type ColorSwatchProps =
  ThemingProps (
    Polymorphic
      ( children   :: Array JSX
      , color      :: MantineColor
      , radius     :: Maybe MantineNumberSize
      , size       :: Maybe Pixels
      , withShadow :: Boolean
      )
  )

type ColorSwatchPropsImpl =
  ThemingPropsImpl (
    PolymorphicImpl
      ( children   :: Array JSX
      , color      :: String
      , radius     :: Nullable MantineNumberSizeImpl
      , size       :: Nullable Pixels
      , withShadow :: Boolean
      )
  )
