module Mantine.Core.DataDisplay.Image
  ( image
  , ImageProps
  , ImageFit(..)
  ) where

import Mantine.Core.Prelude

image :: (ImageProps -> ImageProps) -> JSX
image = mkComponentWithDefault imageComponent defaultImageProps

foreign import imageComponent :: ReactComponent ImagePropsImpl

defaultImageProps :: ImageProps
defaultImageProps = defaultThemingProps { onError: handler_ (pure unit) }

type ImageProps =
  ThemingProps
    ( fallbackSrc :: Maybe String
    , fit         :: Maybe ImageFit
    , onError     :: EventHandler
    , radius      :: Maybe MantineNumberSize
    , src         :: Maybe String
    )

data ImageFit
  = ImageFitContain
  | ImageFitFill
  | ImageFitMozInitial
  | ImageFitInherit
  | ImageFitInitial
  | ImageFitRevert
  | ImageFitUnset
  | ImageFitNone
  | ImageFitCover
  | ImageFitScaleDown

instance ToFFI ImageFit String where
  toNative = case _ of
    ImageFitContain    -> "contain"
    ImageFitFill       -> "fill"
    ImageFitMozInitial -> "-moz-initial"
    ImageFitInherit    -> "inherit"
    ImageFitInitial    -> "initial"
    ImageFitRevert     -> "revert"
    ImageFitUnset      -> "unset"
    ImageFitNone       -> "none"
    ImageFitCover      -> "cover"
    ImageFitScaleDown  -> "scale-down"

type ImagePropsImpl =
  ThemingPropsImpl
    ( fallbackSrc :: Nullable String
    , fit         :: Nullable String
    , onError     :: EventHandler
    , radius      :: Nullable MantineNumberSizeImpl
    , src         :: Nullable String
    )
