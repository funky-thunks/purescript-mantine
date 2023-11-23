module Mantine.Core.Miscellaneous.Transition
  ( transition
  , Props_Transition
  , Props_TransitionImpl
  ) where

import Mantine.Core.Prelude
import React.Basic.Emotion (Style)

transition
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Transition
  => Union attrsImpl attrsImpl_ Props_TransitionImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
transition = element (unsafeCoerce transitionComponent) <<< toNative

foreign import transitionComponent :: ReactComponent (Record Props_TransitionImpl)

type Props_Transition =
  ( children       :: Style -> JSX
  , duration       :: Milliseconds
  , exitDuration   :: Milliseconds
  , keepMounted    :: Boolean
  , mounted        :: Boolean
  , onEnter        :: Effect Unit
  , onEntered      :: Effect Unit
  , onExit         :: Effect Unit
  , onExited       :: Effect Unit
  , timingFunction :: MantineTransitionTimingFunction
  , transition     :: MantineTransition
  )

type Props_TransitionImpl =
  ( children       :: Style -> JSX
  , duration       :: MillisecondsImpl
  , exitDuration   :: MillisecondsImpl
  , keepMounted    :: Boolean
  , mounted        :: Boolean
  , onEnter        :: Effect Unit
  , onEntered      :: Effect Unit
  , onExit         :: Effect Unit
  , onExited       :: Effect Unit
  , timingFunction :: MantineTransitionTimingFunctionImpl
  , transition     :: MantineTransitionImpl
  )
