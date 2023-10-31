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

-- Not supported properties
--   { loaders :: Partial<Record<(string & {}) | "bars" | "dots" | "oval", MantineLoaderComponent>>
--   }

type LoaderProps =
  MantineComponent
    ( children :: Maybe JSX
    , color    :: Maybe MantineColor
    , size     :: Maybe MantineNumberSize
    , type     :: LoaderType
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
  MantineComponentImpl
    ( children :: Nullable JSX
    , color    :: Nullable String
    , size     :: Nullable MantineNumberSizeImpl
    , type     :: String
    )
