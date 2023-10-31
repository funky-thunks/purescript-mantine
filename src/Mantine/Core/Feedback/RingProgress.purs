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
  ThemingProps
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
  ThemingPropsImpl
    ( label     :: Nullable JSX
    , rootColor :: Nullable String
    , roundCaps :: Boolean
    , sections  :: Array RingProgressSectionImpl
    , size      :: Nullable Number
    , thickness :: Nullable Number
    )

type RingProgressSectionImpl =
  { color        :: String
  , onMouseEnter :: EventHandler
  , onMouseLeave :: EventHandler
  , tooltip      :: Nullable JSX
  , value        :: Number
  }
