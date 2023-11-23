module Mantine.Core.Feedback.Loader
  ( loader
  , loader_
  , Props_Loader
  , LoaderType(..)

  , Props_LoaderImpl
  ) where

import Mantine.Core.Prelude

loader
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Loader
  => Union attrsImpl attrsImpl_ Props_LoaderImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
loader = element (unsafeCoerce loaderComponent) <<< toNative

loader_ :: JSX
loader_ = loader {}

foreign import loaderComponent :: ReactComponent (Record Props_LoaderImpl)

-- Not supported properties
--   { loaders :: Partial<Record<(string & {}) | "bars" | "dots" | "oval", MantineLoaderComponent>>
--   }

type Props_Loader =
  Props_Common
    ( children :: JSX
    , color    :: MantineColor
    , size     :: MantineNumberSize
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

type Props_LoaderImpl =
  Props_CommonImpl
    ( children :: JSX
    , color    :: String
    , size     :: MantineNumberSizeImpl
    , type     :: String
    )
