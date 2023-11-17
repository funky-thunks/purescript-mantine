module Mantine.Core.Navigation.Burger
  ( burger
  , BurgerProps
  ) where

import Mantine.Core.Prelude

burger :: (BurgerProps -> BurgerProps) -> JSX
burger = mkComponentWithDefault burgerComponent defaultBurgerProps

foreign import burgerComponent :: ReactComponent BurgerPropsImpl

type BurgerProps =
  MantineComponent
    ( color                    :: Optional MantineColor
    , onClick                  :: EventHandler
    , opened                   :: Boolean
    , size                     :: Optional MantineNumberSize
    , transitionDuration       :: Optional Milliseconds
    , transitionTimingFunction :: Optional MantineTransitionTimingFunction
    )

defaultBurgerProps :: BurgerProps
defaultBurgerProps = defaultMantineComponent { onClick: handler_ (pure unit) }

type BurgerPropsImpl =
  MantineComponentImpl
    ( color                    :: OptionalImpl MantineColorImpl
    , onClick                  :: EventHandler
    , opened                   :: Boolean
    , size                     :: OptionalImpl MantineNumberSizeImpl
    , transitionDuration       :: OptionalImpl MillisecondsImpl
    , transitionTimingFunction :: OptionalImpl MantineTransitionTimingFunctionImpl
    )
