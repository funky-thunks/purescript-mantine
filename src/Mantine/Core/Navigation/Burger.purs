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
    ( color                    :: Maybe MantineColor
    , onClick                  :: EventHandler
    , opened                   :: Boolean
    , size                     :: Maybe MantineNumberSize
    , transitionDuration       :: Maybe Milliseconds
    , transitionTimingFunction :: Maybe MantineTransitionTimingFunction
    )

defaultBurgerProps :: BurgerProps
defaultBurgerProps = defaultMantineComponent { onClick: handler_ (pure unit) }

type BurgerPropsImpl =
  MantineComponentImpl
    ( color                    :: Nullable MantineColorImpl
    , onClick                  :: EventHandler
    , opened                   :: Boolean
    , size                     :: Nullable MantineNumberSizeImpl
    , transitionDuration       :: Nullable MillisecondsImpl
    , transitionTimingFunction :: Nullable MantineTransitionTimingFunctionImpl
    )
