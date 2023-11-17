module Mantine.Core.Layout.Container
  ( container
  , container_
  , Props_Container
  , Props_ContainerImpl
  ) where

import Mantine.Core.Prelude

container
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Container
  => Union attrsImpl attrsImpl_ Props_ContainerImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
container = element (unsafeCoerce containerComponent) <<< toNative

container_ :: Array JSX -> JSX
container_ children = container { children }

foreign import containerComponent :: ReactComponent (Record Props_ContainerImpl)

type Props_Container =
  Props_Common
    ( children :: Array JSX
    , fluid    :: Boolean
    , size     :: MantineNumberSize
    )

type Props_ContainerImpl =
  Props_CommonImpl
    ( children :: Array JSX
    , fluid    :: Boolean
    , size     :: MantineNumberSizeImpl
    )
