module Mantine.Core.Navigation.Burger
  ( burger
  , BurgerProps
  ) where

import Prelude (pure, unit)
import Mantine.Core.Prelude

burger :: (BurgerProps -> BurgerProps) -> JSX
burger = mkComponentWithDefault burgerComponent defaultBurgerProps

foreign import burgerComponent :: ReactComponent BurgerPropsImpl

type BurgerProps =
  ThemingProps
    ( color              :: Maybe MantineColor
    , onClick            :: EventHandler
    , opened             :: Boolean
    , size               :: Maybe MantineNumberSize
    , transitionDuration :: Maybe Milliseconds
    )

defaultBurgerProps :: BurgerProps
defaultBurgerProps = defaultThemingProps { onClick: handler_ (pure unit) }

type BurgerPropsImpl =
  ThemingPropsImpl
    ( color              :: Nullable String
    , onClick            :: EventHandler
    , opened             :: Boolean
    , size               :: Nullable MantineNumberSizeImpl
    , transitionDuration :: Nullable Number
    )
