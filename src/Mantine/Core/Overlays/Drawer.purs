module Mantine.Core.Overlays.Drawer
  ( drawer
  , DrawerProps
  , DrawerPosition(..)
  ) where

import Mantine.Core.Overlays.Overlay (OverlayProps, OverlayPropsImpl)
import Mantine.Core.Overlays.Modal (ModalTransitionProps, ModalTransitionPropsImpl)
import Mantine.Core.Prelude
import Mantine.Core.Buttons.CloseButton (CloseButtonProps, CloseButtonPropsImpl)

drawer :: (DrawerProps -> DrawerProps) -> JSX
drawer = mkComponentWithDefault drawerComponent defaultDrawerProps

foreign import drawerComponent :: ReactComponent DrawerPropsImpl

-- Not supported properties
--   { portalProps         :: Omit<PortalProps, "children">
--   , scrollAreaComponent :: ScrollAreaComponent
--   }

type DrawerProps =
  MantineComponent
    ( children            :: Array JSX
    , closeButtonProps    :: Maybe CloseButtonProps
    , closeOnClickOutside :: Boolean
    , closeOnEscape       :: Boolean
    , id                  :: Maybe String
    , keepMounted         :: Boolean
    , lockScroll          :: Boolean
    , onClose             :: Effect Unit
    , opened              :: Boolean
    , overlayProps        :: OverlayProps
    , padding             :: Maybe MantineNumberSize
    , position            :: DrawerPosition
    , returnFocus         :: Boolean
    , shadow              :: Maybe MantineShadow
    , size                :: Maybe MantineNumberSize
    , title               :: Maybe JSX
    , transitionProps     :: ModalTransitionProps
    , trapFocus           :: Boolean
    , withCloseButton     :: Boolean
    , withOverlay         :: Boolean
    , withinPortal        :: Boolean
    , zIndex              :: Maybe ZIndex
    )

defaultDrawerProps :: DrawerProps
defaultDrawerProps = defaultMantineComponent { onClose: pure unit }

data DrawerPosition
  = DrawerPositionBottom
  | DrawerPositionLeft
  | DrawerPositionRight
  | DrawerPositionTop

instance DefaultValue DrawerPosition where
  defaultValue = DrawerPositionLeft

type DrawerPositionImpl = String

instance ToFFI DrawerPosition DrawerPositionImpl where
  toNative = case _ of
    DrawerPositionBottom -> "bottom"
    DrawerPositionLeft   -> "left"
    DrawerPositionRight  -> "right"
    DrawerPositionTop    -> "top"

type DrawerPropsImpl =
  MantineComponentImpl
    ( children            :: Array JSX
    , closeButtonProps    :: Nullable CloseButtonPropsImpl
    , closeOnClickOutside :: Boolean
    , closeOnEscape       :: Boolean
    , id                  :: Nullable String
    , keepMounted         :: Boolean
    , lockScroll          :: Boolean
    , onClose             :: Effect Unit
    , opened              :: Boolean
    , overlayProps        :: OverlayPropsImpl
    , padding             :: Nullable MantineNumberSizeImpl
    , position            :: DrawerPositionImpl
    , returnFocus         :: Boolean
    , shadow              :: Nullable MantineShadowImpl
    , size                :: Nullable MantineNumberSizeImpl
    , title               :: Nullable JSX
    , transitionProps     :: ModalTransitionPropsImpl
    , trapFocus           :: Boolean
    , withCloseButton     :: Boolean
    , withOverlay         :: Boolean
    , withinPortal        :: Boolean
    , zIndex              :: Nullable ZIndexImpl
    )
