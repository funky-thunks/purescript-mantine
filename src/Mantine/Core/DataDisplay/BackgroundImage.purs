module Mantine.Core.DataDisplay.BackgroundImage
  ( backgroundImage
  , backgroundImage_
  , BackgroundImageProps
  ) where

import Mantine.Core.Prelude

backgroundImage :: String -> (BackgroundImageProps -> BackgroundImageProps) -> JSX
backgroundImage = mkComponentWithDefault backgroundImageComponent <<< defaultBackgroundImageProps

backgroundImage_ :: String -> JSX
backgroundImage_ src = backgroundImage src identity

foreign import backgroundImageComponent:: ReactComponent BackgroundImagePropsImpl

type BackgroundImageProps =
  ThemingProps
    ( radius :: MantineNumberSize
    , src    :: String
    )

defaultBackgroundImageProps :: String -> BackgroundImageProps
defaultBackgroundImageProps src =
  defaultThemingProps
    { radius: Preset Small
    , src
    }

type BackgroundImagePropsImpl =
  ThemingPropsImpl
    ( radius :: MantineNumberSizeImpl
    , src    :: String
    )
