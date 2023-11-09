module Mantine.Core.Overlays.Modal
  ( modal
  , modal_
  , ModalProps

  , drawer
  , DrawerProps
  , DrawerPosition(..)

  , ModalComponent
  , ModalNonDefaultable
  , ModalTransitionProps
  , ModalTransitionPropsImpl
  ) where

import Mantine.Core.Buttons.CloseButton (CloseButtonProps, CloseButtonPropsImpl)
import Mantine.Core.Overlays.Overlay (OverlayProps, OverlayPropsImpl)
import Mantine.Core.Prelude

modal :: (ModalProps -> ModalProps) -> JSX
modal = mkComponentWithDefault modalComponent defaultModalProps

modal_ :: Array JSX -> JSX
modal_ children = modal _ { children = children }

foreign import modalComponent :: ReactComponent ModalPropsImpl

-- Not supported properties
--    { closeButtonProps    :: ModalBaseCloseButtonProps
--    , portalProps         :: Omit<PortalProps, "children">
--    , scrollAreaComponent :: ScrollAreaComponent
--    , xOffset             :: MarginLeft<string | number>
--    , yOffset             :: MarginTop<string | number>
--    }

type ModalProps =
  ModalComponent
    ( centered   :: Boolean
    , fullScreen :: Boolean
    , radius     :: Maybe MantineNumberSize
    )

defaultModalProps :: ModalProps
defaultModalProps = defaultMantineComponent defaultModalComponent

type ModalPropsImpl =
  ModalComponentImpl
    ( centered   :: Boolean
    , fullScreen :: Boolean
    , radius     :: Nullable MantineNumberSizeImpl
    )

drawer :: (DrawerProps -> DrawerProps) -> JSX
drawer = mkComponentWithDefault drawerComponent defaultDrawerProps

foreign import drawerComponent :: ReactComponent DrawerPropsImpl

-- Not supported properties
--   { portalProps         :: Omit<PortalProps, "children">
--   , scrollAreaComponent :: ScrollAreaComponent
--   }

type DrawerProps =
  ModalComponent
    ( closeButtonProps :: Maybe CloseButtonProps
    , position         :: DrawerPosition
    )

defaultDrawerProps :: DrawerProps
defaultDrawerProps = defaultMantineComponent defaultModalComponent

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
  ModalComponentImpl
    ( closeButtonProps :: Nullable CloseButtonPropsImpl
    , position         :: DrawerPositionImpl
    )

type ModalComponent rest =
  MantineComponent
    ( children        :: Array JSX
    , id              :: Maybe String
    , keepMounted     :: Boolean
    , opened          :: Boolean
    , overlayProps    :: OverlayProps
    , padding         :: Maybe MantineNumberSize
    , shadow          :: Maybe MantineShadow
    , size            :: Maybe Dimension
    , title           :: Maybe JSX
    , transitionProps :: ModalTransitionProps
    , zIndex          :: Maybe ZIndex
    | ModalNonDefaultable rest
    )

type ModalNonDefaultable rest =
  ( closeOnClickOutside :: Boolean
  , closeOnEscape       :: Boolean
  , lockScroll          :: Boolean
  , onClose             :: Effect Unit
  , returnFocus         :: Boolean
  , trapFocus           :: Boolean
  , withCloseButton     :: Boolean
  , withOverlay         :: Boolean
  , withinPortal        :: Boolean
  | rest
  )

defaultModalComponent :: Record (ModalNonDefaultable ())
defaultModalComponent =
  { closeOnClickOutside: true
  , closeOnEscape:       true
  , lockScroll:          true
  , onClose:             pure unit
  , returnFocus:         true
  , trapFocus:           true
  , withCloseButton:     true
  , withOverlay:         true
  , withinPortal:        true
  }

type ModalComponentImpl rest =
  MantineComponentImpl
    ( children        :: Array JSX
    , id              :: Nullable String
    , keepMounted     :: Boolean
    , opened          :: Boolean
    , overlayProps    :: OverlayPropsImpl
    , padding         :: Nullable MantineNumberSizeImpl
    , shadow          :: Nullable MantineShadowImpl
    , size            :: Nullable DimensionImpl
    , title           :: Nullable JSX
    , transitionProps :: ModalTransitionPropsImpl
    , zIndex          :: Nullable ZIndexImpl
    | ModalNonDefaultable rest
    )

type ModalTransitionProps     = MantineTransitionBase     (exitDuration :: Maybe    Milliseconds)
type ModalTransitionPropsImpl = MantineTransitionBaseImpl (exitDuration :: Nullable Number      )
