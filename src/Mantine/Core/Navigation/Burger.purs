module Mantine.Core.Navigation.Burger
  ( burger
  , Props_Burger
  , Props_BurgerImpl
  ) where

import Mantine.Core.Prelude

burger
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Burger
  => Union attrsImpl attrsImpl_ Props_BurgerImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
burger = element (unsafeCoerce burgerComponent) <<< toNative

foreign import burgerComponent :: ReactComponent (Record Props_BurgerImpl)

type Props_Burger =
  Props_Common
    ( color                    :: MantineColor
    , onClick                  :: EventHandler
    , opened                   :: Boolean
    , size                     :: MantineNumberSize
    , transitionDuration       :: Milliseconds
    , transitionTimingFunction :: MantineTransitionTimingFunction
    )

type Props_BurgerImpl =
  Props_CommonImpl
    ( color                    :: MantineColorImpl
    , onClick                  :: EventHandler
    , opened                   :: Boolean
    , size                     :: MantineNumberSizeImpl
    , transitionDuration       :: MillisecondsImpl
    , transitionTimingFunction :: MantineTransitionTimingFunctionImpl
    )
