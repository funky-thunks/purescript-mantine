module Mantine.Core.Feedback.RingProgress
  ( ringProgress
  , RingProgressProps
  , RingProgressSection

  , module Mantine.Core.Common
  ) where

import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Mantine.Core.Common (MantineColor(..))
import Mantine.Core.Common as MC
import Mantine.FFI (toNative)
import React.Basic (ReactComponent, element)
import React.Basic.Events (EventHandler)
import React.Basic.Hooks (JSX)

ringProgress :: (RingProgressProps -> RingProgressProps) -> JSX
ringProgress setProps = element ringProgressComponent (toNative (setProps MC.defaultThemingProps_))

foreign import ringProgressComponent :: ReactComponent RingProgressPropsImpl

type RingProgressProps =
  MC.ThemingProps
    ( label     :: Maybe String
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
  MC.ThemingPropsImpl
    ( label     :: Nullable String
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
