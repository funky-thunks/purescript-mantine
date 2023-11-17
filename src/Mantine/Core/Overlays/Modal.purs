module Mantine.Core.Overlays.Modal
  ( modal
  , modal_
  , ModalProps
  , SubModalProps
  , ModalProps_

  , drawer
  , DrawerProps
  , DrawerPosition(..)

  , ModalComponent
  , ModalComponentImpl
  , ModalNonDefaultable
  , ModalTransitionProps
  , ModalTransitionPropsImpl
  , SubModalPropsImpl
  , ModalPropsImpl_
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

type ModalProps = ModalProps_ (children :: Array JSX)
type SubModalProps = ModalProps_ ()
type ModalProps_ rest =
  ModalComponent
    ( centered   :: Boolean
    , fullScreen :: Boolean
    , radius     :: Optional MantineNumberSize
    | rest
    )

defaultModalProps :: ModalProps
defaultModalProps = defaultMantineComponent defaultModalComponent

type ModalPropsImpl = ModalPropsImpl_ (children :: Array JSX)
type SubModalPropsImpl = ModalPropsImpl_ ()
type ModalPropsImpl_ rest =
  ModalComponentImpl
    ( centered   :: Boolean
    , fullScreen :: Boolean
    , radius     :: OptionalImpl MantineNumberSizeImpl
    | rest
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
    ( children         :: Array JSX
    , closeButtonProps :: Optional CloseButtonProps
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
    ( children         :: Array JSX
    , closeButtonProps :: OptionalImpl CloseButtonPropsImpl
    , position         :: DrawerPositionImpl
    )

type ModalComponent rest =
  MantineComponent
    ( id              :: Optional String
    , keepMounted     :: Boolean
    , opened          :: Boolean
    , overlayProps    :: OverlayProps
    , padding         :: Optional MantineNumberSize
    , shadow          :: Optional MantineShadow
    , size            :: Optional Dimension
    , title           :: Optional JSX
    , transitionProps :: ModalTransitionProps
    , zIndex          :: Optional ZIndex
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
    ( id              :: OptionalImpl String
    , keepMounted     :: Boolean
    , opened          :: Boolean
    , overlayProps    :: OverlayPropsImpl
    , padding         :: OptionalImpl MantineNumberSizeImpl
    , shadow          :: OptionalImpl MantineShadowImpl
    , size            :: OptionalImpl DimensionImpl
    , title           :: OptionalImpl JSX
    , transitionProps :: ModalTransitionPropsImpl
    , zIndex          :: OptionalImpl ZIndexImpl
    | ModalNonDefaultable rest
    )

type ModalTransitionProps     = MantineTransitionBase     (exitDuration :: Optional    Milliseconds)
type ModalTransitionPropsImpl = MantineTransitionBaseImpl (exitDuration :: UndefinedOr Number      )
