module Mantine.Core.Layout.Center
  ( center
  , center_
  , CenterProps
  ) where

import Mantine.Core.Prelude

center :: (CenterProps -> CenterProps) -> JSX
center = mkTrivialComponent centerComponent

center_ :: Array JSX -> JSX
center_ children = center _ { children = children }

foreign import centerComponent :: ReactComponent CenterPropsImpl

type CenterProps =
  MantineComponent
    ( children :: Array JSX
    , inline   :: Boolean
    )

type CenterPropsImpl =
  MantineComponentImpl
    ( children :: Array JSX
    , inline   :: Boolean
    )
