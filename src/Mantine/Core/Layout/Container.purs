module Mantine.Core.Layout.Container
  ( container
  , container_
  , ContainerProps
  , ContainerSizes

  , module Mantine.Core.Common
  ) where

import Prelude
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Tuple (Tuple)
import Foreign.Object (Object, fromFoldable)
import Mantine.Core.Common (MantineColor, MantineNumberSize, MantineSize)
import Mantine.Core.Common as MC
import Mantine.FFI (toNative)
import React.Basic (ReactComponent, element)
import React.Basic.Hooks (JSX)
import Record (delete, union)
import Type.Proxy (Proxy(..))

container :: (ContainerProps -> ContainerProps) -> JSX
container setProps = element containerComponent (centerToImpl (setProps MC.defaultThemingProps_))

container_ :: Array JSX -> JSX
container_ children = container _ { children = children }

foreign import containerComponent :: ReactComponent ContainerPropsImpl

type ContainerProps =
  MC.ThemingProps
    ( children :: Array JSX
    , fluid    :: Boolean
    , size     :: Maybe MantineNumberSize
    , sizes    :: ContainerSizes
    )

type ContainerSizes = Array (Tuple MantineSize Number)

type ContainerPropsImpl =
  MC.ThemingPropsImpl
    ( children :: Array JSX
    , fluid    :: Boolean
    , size     :: Nullable MC.MantineNumberSizeImpl
    , sizes    :: Object Number
    )

centerToImpl :: ContainerProps -> ContainerPropsImpl
centerToImpl props =
  let nativeSizes = fromFoldable <<< map (lmap toNative)
   in toNative (delete (Proxy :: Proxy "sizes") props) `union` { sizes: nativeSizes props.sizes }
