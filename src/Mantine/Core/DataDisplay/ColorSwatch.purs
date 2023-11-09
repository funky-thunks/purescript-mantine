module Mantine.Core.DataDisplay.ColorSwatch
  ( colorSwatch
  , colorSwatch_
  , ColorSwatchProps
  ) where

import Mantine.Core.Prelude

colorSwatch :: MantineColor -> (ColorSwatchProps -> ColorSwatchProps) -> JSX
colorSwatch = mkComponent colorSwatchComponent toNative <<< defaultColorSwatch

colorSwatch_ :: MantineColor -> JSX
colorSwatch_ c = colorSwatch c identity

foreign import colorSwatchComponent :: ReactComponent ColorSwatchPropsImpl

defaultColorSwatch :: MantineColor -> ColorSwatchProps
defaultColorSwatch color = defaultMantineComponent { color }

type ColorSwatchProps =
  MantineComponent (
    Polymorphic
      ( children   :: Array JSX
      , color      :: MantineColor
      , radius     :: Maybe MantineNumberSize
      , size       :: Maybe Pixels
      , withShadow :: Boolean
      )
  )

type ColorSwatchPropsImpl =
  MantineComponentImpl (
    PolymorphicImpl
      ( children   :: Array JSX
      , color      :: MantineColorImpl
      , radius     :: Nullable MantineNumberSizeImpl
      , size       :: Nullable PixelsImpl
      , withShadow :: Boolean
      )
  )
