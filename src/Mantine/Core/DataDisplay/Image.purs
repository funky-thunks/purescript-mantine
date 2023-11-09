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
    ( fallbackSrc :: Maybe String
    , fit         :: Maybe ObjectFit
    , onError     :: EventHandler
    , radius      :: Maybe MantineNumberSize
    , src         :: Maybe String
    )

type ImagePropsImpl =
  MantineComponentImpl
    ( fallbackSrc :: Nullable String
    , fit         :: Nullable ObjectFitImpl
    , onError     :: EventHandler
    , radius      :: Nullable MantineNumberSizeImpl
    , src         :: Nullable String
    )
