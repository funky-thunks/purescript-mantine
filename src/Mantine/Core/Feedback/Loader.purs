module Mantine.Core.Feedback.Loader
  ( loader
  , loader_
  , LoaderProps
  , LoaderVariant(..)

  , module Mantine.Core.Common
  ) where

import Prelude
import Data.Default (class DefaultValue)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Mantine.Core.Common (MantineColor(..), MantineNumberSize, MantineSize(..))
import Mantine.Core.Common as MC
import Mantine.FFI (class ToFFI, toNative)
import React.Basic (ReactComponent, element)
import React.Basic.Hooks (JSX)

loader :: (LoaderProps -> LoaderProps) -> JSX
loader setProps = element loaderComponent (toNative (setProps MC.defaultThemingProps_))

loader_ :: JSX
loader_ = loader identity

foreign import loaderComponent :: ReactComponent LoaderPropsImpl

type LoaderProps =
  MC.ThemingProps
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
  MC.ThemingPropsImpl
    ( color   :: Nullable String
    , size    :: Nullable MC.MantineNumberSizeImpl
    , variant :: String
    )
