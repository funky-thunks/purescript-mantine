module Mantine.Core.DataDisplay.Image
  ( image
  , ImageProps
  , ImageFit(..)
  ) where

import Mantine.Core.Prelude

image :: (ImageProps -> ImageProps) -> JSX
image = mkTrivialComponent imageComponent

foreign import imageComponent :: ReactComponent ImagePropsImpl

type ImageProps =
  ThemingProps
    ( alt         :: Maybe String
    , caption     :: Maybe JSX
    , fit         :: Maybe ImageFit
    , height      :: Maybe Dimension
    , placeholder :: Maybe JSX
    , radius      :: Maybe MantineNumberSize
    , src         :: Maybe String
    , width       :: Maybe Dimension
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
    ( alt         :: Nullable String
    , caption     :: Nullable JSX
    , fit         :: Nullable String
    , height      :: Nullable DimensionImpl
    , placeholder :: Nullable JSX
    , src         :: Nullable String
    , width       :: Nullable DimensionImpl
    , radius      :: Nullable MantineNumberSizeImpl
    )
