module Mantine.Core.Layout.AspectRatio
  ( aspectRatio
  , AspectRatioProps
  ) where

import Mantine.Core.Common as MC
import Mantine.FFI (toNative)
import React.Basic (ReactComponent, element)
import React.Basic.Hooks (JSX)

aspectRatio :: (AspectRatioProps -> AspectRatioProps) -> JSX
aspectRatio setProps = element aspectRatioComponent (toNative (setProps defaultAspectRatioProps))

foreign import aspectRatioComponent :: ReactComponent AspectRatioPropsImpl

type AspectRatioProps =
  MC.ThemingProps
    ( children :: Array JSX
    , ratio    :: Number
    )

defaultAspectRatioProps :: AspectRatioProps
defaultAspectRatioProps =
  MC.defaultThemingProps
    { children: []
    , ratio: 1.0
    }

type AspectRatioPropsImpl =
  MC.ThemingPropsImpl
    ( children :: Array JSX
    , ratio    :: Number
    )
