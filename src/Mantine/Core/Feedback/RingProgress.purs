module Mantine.Core.Feedback.RingProgress
  ( ringProgress
  , RingProgressProps
  , RingProgressSection
  ) where

import Mantine.Core.Prelude

ringProgress :: (RingProgressProps -> RingProgressProps) -> JSX
ringProgress = mkTrivialComponent ringProgressComponent

foreign import ringProgressComponent :: ReactComponent RingProgressPropsImpl

type RingProgressProps =
  MantineComponent
    ( label     :: Optional JSX
    , rootColor :: Optional MantineColor
    , roundCaps :: Boolean
    , sections  :: Array RingProgressSection
    , size      :: Optional Number
    , thickness :: Optional Number
    )

type RingProgressSection =
  { color        :: MantineColor
  , onMouseEnter :: EventHandler
  , onMouseLeave :: EventHandler
  , tooltip      :: Optional JSX
  , value        :: Number
  }

type RingProgressPropsImpl =
  MantineComponentImpl
    ( label     :: OptionalImpl JSX
    , rootColor :: OptionalImpl MantineColorImpl
    , roundCaps :: Boolean
    , sections  :: Array RingProgressSectionImpl
    , size      :: OptionalImpl Number
    , thickness :: OptionalImpl Number
    )

type RingProgressSectionImpl =
  { color        :: MantineColorImpl
  , onMouseEnter :: EventHandler
  , onMouseLeave :: EventHandler
  , tooltip      :: OptionalImpl JSX
  , value        :: Number
  }
