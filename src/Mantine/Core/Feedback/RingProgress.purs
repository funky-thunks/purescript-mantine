module Mantine.Core.Feedback.RingProgress
  ( ringProgress
  , Props_RingProgress
  , Props_RingProgressImpl
  , RingProgressSection
  , RingProgressSectionImpl
  ) where

import Mantine.Core.Prelude

ringProgress
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_RingProgress
  => Union attrsImpl attrsImpl_ Props_RingProgressImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
ringProgress = element (unsafeCoerce ringProgressComponent) <<< toNative

foreign import ringProgressComponent :: ReactComponent (Record Props_RingProgressImpl)

type Props_RingProgress =
  Props_Common
    ( label     :: JSX
    , rootColor :: MantineColor
    , roundCaps :: Boolean
    , sections  :: Array RingProgressSection
    , size      :: Number
    , thickness :: Number
    )

type RingProgressSection =
  { color        :: MantineColor
  , onMouseEnter :: EventHandler
  , onMouseLeave :: EventHandler
  , tooltip      :: Optional JSX
  , value        :: Number
  }

type Props_RingProgressImpl =
  Props_CommonImpl
    ( label     :: JSX
    , rootColor :: MantineColorImpl
    , roundCaps :: Boolean
    , sections  :: Array RingProgressSectionImpl
    , size      :: Number
    , thickness :: Number
    )

type RingProgressSectionImpl =
  { color        :: MantineColorImpl
  , onMouseEnter :: EventHandler
  , onMouseLeave :: EventHandler
  , tooltip      :: OptionalImpl JSX
  , value        :: Number
  }
