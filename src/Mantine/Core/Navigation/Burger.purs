module Mantine.Core.Navigation.Burger
  ( burger
  , BurgerProps

  , module Mantine.Core.Common
  ) where

import Prelude
import Data.Default (defaultValue)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Mantine.Core.Common (MantineColor(..), MantineNumberSize, MantineSize(..), Milliseconds)
import Mantine.Core.Common as MC
import Mantine.FFI (toNative)
import React.Basic (ReactComponent, element)
import React.Basic.Events (EventHandler, handler_)
import React.Basic.Hooks (JSX)
import Record (union)

burger :: (BurgerProps -> BurgerProps) -> JSX
burger setProps = element burgerComponent (toNative (setProps defaultBurgerProps))

foreign import burgerComponent :: ReactComponent BurgerPropsImpl

type BurgerProps =
  MC.ThemingProps
    ( color              :: Maybe MantineColor
    , onClick            :: EventHandler
    , opened             :: Boolean
    , size               :: Maybe MantineNumberSize
    , transitionDuration :: Maybe Milliseconds
    )

defaultBurgerProps :: BurgerProps
defaultBurgerProps =
  MC.defaultThemingProps
    { onClick: handler_ (pure unit)
    } `union` defaultValue

type BurgerPropsImpl =
  MC.ThemingPropsImpl
    ( color              :: Nullable String
    , onClick            :: EventHandler
    , opened             :: Boolean
    , size               :: Nullable MC.MantineNumberSizeImpl
    , transitionDuration :: Nullable Number
    )
