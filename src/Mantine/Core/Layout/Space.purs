module Mantine.Core.Layout.Space
  ( space
  , SpaceProps

  , module Mantine.Core.Common
  ) where

import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Mantine.Core.Common (MantineNumberSize, MantineSize)
import Mantine.Core.Common as MC
import Mantine.FFI (toNative)
import React.Basic (ReactComponent, element)
import React.Basic.Hooks (JSX)

space :: (SpaceProps -> SpaceProps) -> JSX
space setProps = element spaceComponent (toNative (setProps MC.defaultThemingProps_))

foreign import spaceComponent :: ReactComponent SpacePropsImpl

type SpaceProps =
  MC.ThemingProps
    ( h :: Maybe MantineNumberSize
    , w :: Maybe MantineNumberSize
    )

type SpacePropsImpl =
  MC.ThemingPropsImpl
    ( h :: Nullable MC.MantineNumberSizeImpl
    , w :: Nullable MC.MantineNumberSizeImpl
    )
