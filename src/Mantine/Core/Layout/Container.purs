module Mantine.Core.Layout.Container
  ( container
  , container_
  , ContainerProps
  , ContainerSizes
  ) where

import Prelude
import Data.Bifunctor (lmap)
import Mantine.Core.Prelude

container :: (ContainerProps -> ContainerProps) -> JSX
container = mkComponent containerComponent centerToImpl defaultThemingProps_

container_ :: Array JSX -> JSX
container_ children = container _ { children = children }

foreign import containerComponent :: ReactComponent ContainerPropsImpl

type ContainerProps =
  ThemingProps
    ( children :: Array JSX
    , fluid    :: Boolean
    , size     :: Maybe MantineNumberSize
    , sizes    :: ContainerSizes
    )

type ContainerSizes = Array (Tuple MantineSize Number)

type ContainerPropsImpl =
  ThemingPropsImpl
    ( children :: Array JSX
    , fluid    :: Boolean
    , size     :: Nullable MantineNumberSizeImpl
    , sizes    :: Object Number
    )

centerToImpl :: ContainerProps -> ContainerPropsImpl
centerToImpl props =
  let nativeSizes = fromFoldable <<< map (lmap toNative)
   in toNative (delete (Proxy :: Proxy "sizes") props) `union` { sizes: nativeSizes props.sizes }
