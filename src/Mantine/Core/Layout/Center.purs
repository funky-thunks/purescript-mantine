module Mantine.Core.Layout.Center
  ( center
  , center_
  , CenterProps

  , module Mantine.Core.Common
  ) where

import Mantine.Core.Common (MantineColor(..), MantineSize(..), MantineGradient, Orientation(..), Radius(..))
import Mantine.Core.Common as MC
import Mantine.FFI (toNative)
import React.Basic (ReactComponent, element)
import React.Basic.Hooks (JSX)

center :: (CenterProps -> CenterProps) -> JSX
center setProps = element centerComponent (toNative (setProps MC.defaultThemingProps_))

center_ :: Array JSX -> JSX
center_ children = center _ { children = children }

foreign import centerComponent :: ReactComponent CenterPropsImpl

type CenterProps =
  MC.ThemingProps
    ( children :: Array JSX
    , inline   :: Boolean
    )

type CenterPropsImpl =
  MC.ThemingPropsImpl
    ( children :: Array JSX
    , inline   :: Boolean
    )
