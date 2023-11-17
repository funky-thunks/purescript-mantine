module Mantine.Core.DataDisplay.Image
  ( image
  , ImageProps
  ) where

import Mantine.Core.Prelude

image :: (ImageProps -> ImageProps) -> JSX
image = mkComponentWithDefault imageComponent defaultImageProps

foreign import imageComponent :: ReactComponent ImagePropsImpl

defaultImageProps :: ImageProps
defaultImageProps = defaultMantineComponent { onError: handler_ (pure unit) }

type ImageProps =
  MantineComponent
    ( fallbackSrc :: Optional String
    , fit         :: Optional ObjectFit
    , onError     :: EventHandler
    , radius      :: Optional MantineNumberSize
    , src         :: Optional String
    )

type ImagePropsImpl =
  MantineComponentImpl
    ( fallbackSrc :: OptionalImpl String
    , fit         :: OptionalImpl ObjectFitImpl
    , onError     :: EventHandler
    , radius      :: OptionalImpl MantineNumberSizeImpl
    , src         :: OptionalImpl String
    )
