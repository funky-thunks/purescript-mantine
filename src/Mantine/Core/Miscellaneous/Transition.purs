module Mantine.Core.Miscellaneous.Transition
  ( transition
  , TransitionProps
  ) where

import Mantine.Core.Prelude
import React.Basic.Emotion (Style)

transition :: (Style -> JSX) -> (TransitionProps -> TransitionProps) -> JSX
transition = mkComponent transitionComponent transitionPropsToImpl <<< defaultTransitionProps

foreign import transitionComponent :: ReactComponent TransitionPropsImpl

type TransitionProps =
  { children       :: Style -> JSX
  , duration       :: Maybe Number
  , exitDuration   :: Maybe Number
  , mounted        :: Boolean
  , onEnter        :: Effect Unit
  , onEntered      :: Effect Unit
  , onExit         :: Effect Unit
  , onExited       :: Effect Unit
  , timingFunction :: Maybe String
  , transition     :: Maybe MantineTransition
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
  , duration       :: Nullable Number
  , exitDuration   :: Nullable Number
  , mounted        :: Boolean
  , onEnter        :: Effect Unit
  , onEntered      :: Effect Unit
  , onExit         :: Effect Unit
  , onExited       :: Effect Unit
  , timingFunction :: Nullable String
  , transition     :: Nullable String
  }

transitionPropsToImpl :: TransitionProps -> TransitionPropsImpl
transitionPropsToImpl props =
  let rest = toNative
         <<< delete (Proxy :: Proxy "children")
   in { children: props.children } `union` rest props

-- TODO
-- groupedTransition :: (GroupedTransitionProps -> GroupedTransitionProps) -> JSX
-- groupedTransition = mkTrivialComponent groupedTransitionComponent
