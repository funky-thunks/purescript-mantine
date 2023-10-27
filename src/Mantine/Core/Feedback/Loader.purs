module Mantine.Core.Feedback.Loader
  ( loader
  , loader_
  , LoaderProps
  , LoaderVariant(..)
  ) where

import Mantine.Core.Prelude

loader :: (LoaderProps -> LoaderProps) -> JSX
loader = mkTrivialComponent loaderComponent

loader_ :: JSX
loader_ = loader identity

foreign import loaderComponent :: ReactComponent LoaderPropsImpl

type LoaderProps =
  ThemingProps
    ( color    :: Maybe MantineColor
    , size     :: Maybe MantineNumberSize
    , variant  :: LoaderVariant
    )

data LoaderVariant
  = LoaderVariantBars
  | LoaderVariantOval
  | LoaderVariantDots

instance ToFFI LoaderVariant String where
  toNative = case _ of
    LoaderVariantBars -> "bars"
    LoaderVariantOval -> "oval"
    LoaderVariantDots -> "dots"

instance DefaultValue LoaderVariant where
  defaultValue = LoaderVariantOval

type LoaderPropsImpl =
  ThemingPropsImpl
    ( color   :: Nullable String
    , size    :: Nullable MantineNumberSizeImpl
    , variant :: String
    )
