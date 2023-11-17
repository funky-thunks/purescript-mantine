module Mantine.Core.Overlays.Dialog
  ( dialog
  , Props_Dialog
  , Props_DialogImpl
  , DialogPosition
  , DialogPositionImpl
  ) where

import Mantine.Core.Prelude

dialog
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Dialog
  => Union attrsImpl attrsImpl_ Props_DialogImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
dialog = element (unsafeCoerce dialogComponent) <<< toNative

foreign import dialogComponent :: ReactComponent (Record Props_DialogImpl)

-- Not supported properties
--   { portalProps :: Omit<PortalProps, "children">
--   }

type Props_Dialog =
  Props_Common
    ( children        :: Array JSX
    , keepMounted     :: Boolean
    , onClose         :: Effect Unit
    , opened          :: Boolean
    , position        :: DialogPosition
    , radius          :: MantineNumberSize
    , shadow          :: MantineShadow
    , size            :: Dimension
    , transitionProps :: MantineTransitionProps
    , withBorder      :: Boolean
    , withCloseButton :: Boolean
    , withinPortal    :: Boolean
    , zIndex          :: ZIndex
    )

type DialogPosition =
  { bottom :: Optional Dimension
  , left   :: Optional Dimension
  , right  :: Optional Dimension
  , top    :: Optional Dimension
  }

type Props_DialogImpl =
  Props_CommonImpl
    ( children        :: Array JSX
    , keepMounted     :: Boolean
    , onClose         :: Effect Unit
    , opened          :: Boolean
    , position        :: DialogPositionImpl
    , radius          :: MantineNumberSizeImpl
    , shadow          :: MantineShadowImpl
    , size            :: DimensionImpl
    , transitionProps :: MantineTransitionPropsImpl
    , withBorder      :: Boolean
    , withCloseButton :: Boolean
    , withinPortal    :: Boolean
    , zIndex          :: ZIndexImpl
    )

type DialogPositionImpl =
  { bottom :: OptionalImpl DimensionImpl
  , left   :: OptionalImpl DimensionImpl
  , right  :: OptionalImpl DimensionImpl
  , top    :: OptionalImpl DimensionImpl
  }
