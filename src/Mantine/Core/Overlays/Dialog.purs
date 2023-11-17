module Mantine.Core.Overlays.Dialog
  ( dialog
  , DialogProps
  , DialogPosition
  ) where

import Mantine.Core.Prelude

dialog :: (DialogProps -> DialogProps) -> JSX
dialog = mkComponentWithDefault dialogComponent defaultDialogProps

foreign import dialogComponent :: ReactComponent DialogPropsImpl

-- Not supported properties
--   { portalProps :: Omit<PortalProps, "children">
--   }

type DialogProps =
  MantineComponent
    ( children        :: Array JSX
    , keepMounted     :: Boolean
    , onClose         :: Effect Unit
    , opened          :: Optional Boolean
    , position        :: Optional DialogPosition
    , radius          :: Optional MantineNumberSize
    , shadow          :: Optional MantineShadow
    , size            :: Optional Dimension
    , transitionProps :: MantineTransitionProps
    , withBorder      :: Boolean
    , withCloseButton :: Optional Boolean
    , withinPortal    :: Optional Boolean
    , zIndex          :: Optional ZIndex
    )

type DialogPosition =
  { bottom :: Optional Dimension
  , left   :: Optional Dimension
  , right  :: Optional Dimension
  , top    :: Optional Dimension
  }

defaultDialogProps :: DialogProps
defaultDialogProps = defaultMantineComponent { onClose: pure unit }

type DialogPropsImpl =
  MantineComponentImpl
    ( children        :: Array JSX
    , keepMounted     :: Boolean
    , onClose         :: Effect Unit
    , opened          :: OptionalImpl Boolean
    , position        :: OptionalImpl DialogPositionImpl
    , radius          :: OptionalImpl MantineNumberSizeImpl
    , shadow          :: OptionalImpl MantineShadowImpl
    , size            :: OptionalImpl DimensionImpl
    , transitionProps :: MantineTransitionPropsImpl
    , withBorder      :: Boolean
    , withCloseButton :: OptionalImpl Boolean
    , withinPortal    :: OptionalImpl Boolean
    , zIndex          :: OptionalImpl ZIndexImpl
    )

type DialogPositionImpl =
  { bottom :: OptionalImpl DimensionImpl
  , left   :: OptionalImpl DimensionImpl
  , right  :: OptionalImpl DimensionImpl
  , top    :: OptionalImpl DimensionImpl
  }
