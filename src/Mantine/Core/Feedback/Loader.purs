module Mantine.Core.Feedback.Loader
  ( loader
  , loader_
  , LoaderProps
  , LoaderType(..)

  , LoaderPropsImpl
  ) where

import Mantine.Core.Prelude

loader :: (LoaderProps -> LoaderProps) -> JSX
loader = mkTrivialComponent loaderComponent

loader_ :: JSX
loader_ = loader identity

foreign import loaderComponent :: ReactComponent LoaderPropsImpl

type LoaderProps =
  ThemingProps
    ( color :: Maybe MantineColor
    , size  :: Maybe MantineNumberSize
    , type  :: LoaderType
    )

data LoaderType
  = LoaderTypeBars
  | LoaderTypeOval
  | LoaderTypeDots
  | LoaderTypeCustom String

instance ToFFI LoaderType String where
  toNative = case _ of
    LoaderTypeBars     -> "bars"
    LoaderTypeOval     -> "oval"
    LoaderTypeDots     -> "dots"
    LoaderTypeCustom s -> s

instance DefaultValue LoaderType where
  defaultValue = LoaderTypeOval

type LoaderPropsImpl =
  ThemingPropsImpl
    ( color :: Nullable String
    , size  :: Nullable MantineNumberSizeImpl
    , type  :: String
    )
