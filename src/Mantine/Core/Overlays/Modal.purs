module Mantine.Core.Overlays.Modal
  ( modal
  , modal_
  , ModalProps
  , ModalTransitionProps

  , ModalTransitionPropsImpl
  ) where

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
  MantineComponent
    ( centered            :: Boolean
    , children            :: Array JSX
    , closeOnClickOutside :: Boolean
    , closeOnEscape       :: Boolean
    , fullScreen          :: Boolean
    , id                  :: Maybe String
    , keepMounted         :: Boolean
    , lockScroll          :: Boolean
    , onClose             :: Effect Unit
    , opened              :: Boolean
    , overlayProps        :: OverlayProps
    , padding             :: Maybe MantineNumberSize
    , radius              :: Maybe MantineNumberSize
    , returnFocus         :: Boolean
    , shadow              :: Maybe MantineShadow
    , size                :: Maybe Dimension
    , title               :: Maybe JSX
    , transitionProps     :: ModalTransitionProps
    , trapFocus           :: Boolean
    , withCloseButton     :: Boolean
    , withOverlay         :: Boolean
    , withinPortal        :: Boolean
    , zIndex              :: Maybe ZIndex
    )

defaultModalProps :: ModalProps
defaultModalProps =
  defaultMantineComponent
    { closeOnClickOutside: true
    , closeOnEscape:       true
    , lockScroll:          true
    , onClose:             pure unit
    , returnFocus:         true
    , withCloseButton:     true
    , withOverlay:         true
    , withinPortal:        true
    }

type ModalPropsImpl =
  MantineComponentImpl
    ( centered            :: Boolean
    , children            :: Array JSX
    , closeOnClickOutside :: Boolean
    , closeOnEscape       :: Boolean
    , fullScreen          :: Boolean
    , id                  :: Nullable String
    , keepMounted         :: Boolean
    , lockScroll          :: Boolean
    , onClose             :: Effect Unit
    , opened              :: Boolean
    , overlayProps        :: OverlayPropsImpl
    , padding             :: Nullable MantineNumberSizeImpl
    , radius              :: Nullable MantineNumberSizeImpl
    , returnFocus         :: Boolean
    , shadow              :: Nullable MantineShadowImpl
    , size                :: Nullable DimensionImpl
    , title               :: Nullable JSX
    , transitionProps     :: ModalTransitionPropsImpl
    , trapFocus           :: Boolean
    , withCloseButton     :: Boolean
    , withOverlay         :: Boolean
    , withinPortal        :: Boolean
    , zIndex              :: Nullable ZIndexImpl
    )

type ModalTransitionProps     = MantineTransitionBase     (exitDuration :: Maybe    Milliseconds)
type ModalTransitionPropsImpl = MantineTransitionBaseImpl (exitDuration :: Nullable Number      )
