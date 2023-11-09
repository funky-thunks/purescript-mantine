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
    , opened          :: Maybe Boolean
    , position        :: Maybe DialogPosition
    , radius          :: Maybe MantineNumberSize
    , shadow          :: Maybe MantineShadow
    , size            :: Maybe Dimension
    , transitionProps :: MantineTransitionProps
    , withBorder      :: Boolean
    , withCloseButton :: Maybe Boolean
    , withinPortal    :: Maybe Boolean
    , zIndex          :: Maybe ZIndex
    )

type DialogPosition =
  { bottom :: Maybe Dimension
  , left   :: Maybe Dimension
  , right  :: Maybe Dimension
  , top    :: Maybe Dimension
  }

defaultDialogProps :: DialogProps
defaultDialogProps = defaultMantineComponent { onClose: pure unit }

type DialogPropsImpl =
  MantineComponentImpl
    ( children        :: Array JSX
    , keepMounted     :: Boolean
    , onClose         :: Effect Unit
    , opened          :: Nullable Boolean
    , position        :: Nullable DialogPositionImpl
    , radius          :: Nullable MantineNumberSizeImpl
    , shadow          :: Nullable MantineShadowImpl
    , size            :: Nullable DimensionImpl
    , transitionProps :: MantineTransitionPropsImpl
    , withBorder      :: Boolean
    , withCloseButton :: Nullable Boolean
    , withinPortal    :: Nullable Boolean
    , zIndex          :: Nullable ZIndexImpl
    )

type DialogPositionImpl =
  { bottom :: Nullable DimensionImpl
  , left   :: Nullable DimensionImpl
  , right  :: Nullable DimensionImpl
  , top    :: Nullable DimensionImpl
  }
