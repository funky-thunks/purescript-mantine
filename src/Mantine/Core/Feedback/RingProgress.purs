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
    ( label     :: Maybe JSX
    , rootColor :: Maybe MantineColor
    , roundCaps :: Boolean
    , sections  :: Array RingProgressSection
    , size      :: Maybe Number
    , thickness :: Maybe Number
    )

type RingProgressSection =
  { color        :: MantineColor
  , onMouseEnter :: EventHandler
  , onMouseLeave :: EventHandler
  , tooltip      :: Maybe JSX
  , value        :: Number
  }

type RingProgressPropsImpl =
  MantineComponentImpl
    ( label     :: Nullable JSX
    , rootColor :: Nullable MantineColorImpl
    , roundCaps :: Boolean
    , sections  :: Array RingProgressSectionImpl
    , size      :: Nullable Number
    , thickness :: Nullable Number
    )

type RingProgressSectionImpl =
  { color        :: MantineColorImpl
  , onMouseEnter :: EventHandler
  , onMouseLeave :: EventHandler
  , tooltip      :: Nullable JSX
  , value        :: Number
  }
