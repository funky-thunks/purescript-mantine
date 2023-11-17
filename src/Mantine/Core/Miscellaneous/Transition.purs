module Mantine.Core.Miscellaneous.Transition
  ( transition
  , TransitionProps
  ) where

import Mantine.Core.Prelude
import React.Basic.Emotion (Style)

transition :: (Style -> JSX) -> (TransitionProps -> TransitionProps) -> JSX
transition = mkComponentWithDefault transitionComponent <<< defaultTransitionProps

foreign import transitionComponent :: ReactComponent TransitionPropsImpl

type TransitionProps =
  { children       :: Style -> JSX
  , duration       :: Optional Milliseconds
  , exitDuration   :: Optional Milliseconds
  , keepMounted    :: Boolean
  , mounted        :: Boolean
  , onEnter        :: Effect Unit
  , onEntered      :: Effect Unit
  , onExit         :: Effect Unit
  , onExited       :: Effect Unit
  , timingFunction :: Optional MantineTransitionTimingFunction
  , transition     :: Optional MantineTransition
  }

defaultTransitionProps :: (Style -> JSX) -> TransitionProps
defaultTransitionProps children =
  { children
  , onEnter:   pure unit
  , onEntered: pure unit
  , onExit:    pure unit
  , onExited:  pure unit
  } `union` defaultValue

type TransitionPropsImpl =
  { children       :: Style -> JSX
  , duration       :: OptionalImpl MillisecondsImpl
  , exitDuration   :: OptionalImpl MillisecondsImpl
  , keepMounted    :: Boolean
  , mounted        :: Boolean
  , onEnter        :: Effect Unit
  , onEntered      :: Effect Unit
  , onExit         :: Effect Unit
  , onExited       :: Effect Unit
  , timingFunction :: OptionalImpl MantineTransitionTimingFunctionImpl
  , transition     :: OptionalImpl MantineTransitionImpl
  }
