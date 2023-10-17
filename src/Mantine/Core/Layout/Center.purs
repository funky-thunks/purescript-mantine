module Mantine.Core.Layout.Center
  ( center
  , center_
  , CenterProps
  ) where

import Mantine.Core.Prelude

center :: (CenterProps -> CenterProps) -> JSX
center = mkComponentWithDefault centerComponent defaultThemingProps_

center_ :: Array JSX -> JSX
center_ children = center _ { children = children }

foreign import centerComponent :: ReactComponent CenterPropsImpl

type CenterProps =
  ThemingProps
    ( children :: Array JSX
    , inline   :: Boolean
    )

type CenterPropsImpl =
  ThemingPropsImpl
    ( children :: Array JSX
    , inline   :: Boolean
    )
