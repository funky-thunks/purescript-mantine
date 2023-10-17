module Mantine.Core.Layout.MediaQuery
  ( mediaQuery
  , MediaQueryProps

  , module Mantine.Core.Common
  ) where

import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Mantine.Core.Common (MantineNumberSize, MantineSize(..))
import Mantine.Core.Common as MC
import Mantine.FFI (toNative)
import React.Basic (ReactComponent, JSX, element)

mediaQuery :: (MediaQueryProps -> MediaQueryProps) -> JSX
mediaQuery setProps = element mediaQueryComponent (toNative (setProps MC.defaultThemingProps_))

foreign import mediaQueryComponent :: ReactComponent MediaQueryPropsImpl

type MediaQueryProps =
  MC.ThemingProps
    ( children    :: Array JSX
    , largerThan  :: Maybe MantineNumberSize
    , query       :: Maybe String
    , smallerThan :: Maybe MantineNumberSize
    )

type MediaQueryPropsImpl =
  MC.ThemingPropsImpl
    ( children    :: Array JSX
    , largerThan  :: Nullable MC.MantineNumberSizeImpl
    , query       :: Nullable String
    , smallerThan :: Nullable MC.MantineNumberSizeImpl
    )
