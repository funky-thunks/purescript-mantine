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
    ( label        :: JSX
    , onMouseLeave :: EventHandler
    , rootColor    :: MantineColor
    , roundCaps    :: Boolean
    , sections     :: Array RingProgressSection
    , size         :: Pixels
    , thickness    :: Pixels
    )

type RingProgressSection =
  { color        :: Optional MantineColor
  , onMouseEnter :: Optional EventHandler
  , onMouseLeave :: Optional EventHandler
  , tooltip      :: Optional JSX
  , value        :: Optional Number
  }

type Props_RingProgressImpl =
  Props_CommonImpl
    ( label        :: JSX
    , onMouseLeave :: EventHandler
    , rootColor    :: MantineColorImpl
    , roundCaps    :: Boolean
    , sections     :: Array RingProgressSectionImpl
    , size         :: PixelsImpl
    , thickness    :: PixelsImpl
    )

type RingProgressSectionImpl =
  { color        :: OptionalImpl MantineColorImpl
  , onMouseEnter :: OptionalImpl EventHandler
  , onMouseLeave :: OptionalImpl EventHandler
  , tooltip      :: OptionalImpl JSX
  , value        :: OptionalImpl Number
  }
