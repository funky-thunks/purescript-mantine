module Mantine.Core.Layout.Container
  ( container
  , container_
  , ContainerProps
  ) where

import Mantine.Core.Prelude

container :: (ContainerProps -> ContainerProps) -> JSX
container = mkTrivialComponent containerComponent

container_ :: Array JSX -> JSX
container_ children = container _ { children = children }

foreign import containerComponent :: ReactComponent ContainerPropsImpl

type ContainerProps =
  MantineComponent
    ( children :: Array JSX
    , fluid    :: Boolean
    , size     :: Maybe MantineNumberSize
    )

type ContainerPropsImpl =
  MantineComponentImpl
    ( children :: Array JSX
    , fluid    :: Boolean
    , size     :: Nullable MantineNumberSizeImpl
    )
