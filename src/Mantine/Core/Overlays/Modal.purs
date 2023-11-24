module Mantine.Core.Overlays.Modal
  ( modal
  , modal_
  , Props_Modal
  , Props_ModalImpl
  , Props_SubModal
  , Props_SubModalImpl
  , Props_Modal_
  , Props_ModalImpl_

  , drawer
  , Props_Drawer
  , Props_DrawerImpl
  , DrawerPosition(..)
  , DrawerPositionImpl

  , ModalComponent
  , ModalComponentImpl
  , ModalNonDefaultable
  , ModalTransitionProps
  , ModalTransitionPropsImpl
  ) where

import Mantine.Core.Buttons.CloseButton (Props_CloseButtonInner, Props_CloseButtonInnerImpl)
import Mantine.Core.Overlays.Overlay (Props_OverlayInner, Props_OverlayInnerImpl)
import Mantine.Core.Prelude

modal
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Modal
  => Union attrsImpl attrsImpl_ Props_ModalImpl
  => ToFFI (Record attrs)  (Record attrsImpl)
  => Record attrs -> JSX
modal = element (unsafeCoerce modalComponent) <<< toNative

modal_ :: Array JSX -> JSX
modal_ children = modal { children }

foreign import modalComponent :: ReactComponent (Record Props_ModalImpl)

-- Not supported properties
--    { closeButtonProps    :: ModalBaseCloseButtonProps
--    , portalProps         :: Omit<PortalProps, "children">
--    , scrollAreaComponent :: ScrollAreaComponent
--    , xOffset             :: MarginLeft<string | number>
--    , yOffset             :: MarginTop<string | number>
--    }

type Props_Modal    = Props_Modal_ (children :: Array JSX)
type Props_SubModal = Props_Modal_ ()
type Props_Modal_ rest =
  ModalComponent
    ( centered   :: Boolean
    , fullScreen :: Boolean
    , radius     :: MantineNumberSize
    | rest
    )

type Props_ModalImpl = Props_ModalImpl_ (children :: Array JSX)
type Props_SubModalImpl = Props_ModalImpl_ ()
type Props_ModalImpl_ rest =
  ModalComponentImpl
    ( centered   :: Boolean
    , fullScreen :: Boolean
    , radius     :: MantineNumberSizeImpl
    | rest
    )

drawer
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Drawer
  => Union attrsImpl attrsImpl_ Props_DrawerImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
drawer = element (unsafeCoerce drawerComponent) <<< toNative

foreign import drawerComponent :: ReactComponent (Record Props_DrawerImpl)

-- Not supported properties
--   { portalProps         :: Omit<PortalProps, "children">
--   , scrollAreaComponent :: ScrollAreaComponent
--   }

type Props_Drawer =
  ModalComponent
    ( children         :: Array JSX
    , closeButtonProps :: Record Props_CloseButtonInner
    , position         :: DrawerPosition
    )

data DrawerPosition
  = DrawerPositionBottom
  | DrawerPositionLeft
  | DrawerPositionRight
  | DrawerPositionTop

type DrawerPositionImpl = String

instance ToFFI DrawerPosition DrawerPositionImpl where
  toNative = case _ of
    DrawerPositionBottom -> "bottom"
    DrawerPositionLeft   -> "left"
    DrawerPositionRight  -> "right"
    DrawerPositionTop    -> "top"

type Props_DrawerImpl =
  ModalComponentImpl
    ( children         :: Array JSX
    , closeButtonProps :: Record Props_CloseButtonInnerImpl
    , position         :: DrawerPositionImpl
    )

type ModalComponent rest =
  Props_Common
    ( id              :: String
    , keepMounted     :: Boolean
    , opened          :: Boolean
    , overlayProps    :: Record Props_OverlayInner
    , padding         :: MantineNumberSize
    , shadow          :: MantineShadow
    , size            :: Dimension
    , title           :: JSX
    , transitionProps :: ModalTransitionProps
    , zIndex          :: ZIndex
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

type ModalComponentImpl rest =
  Props_CommonImpl
    ( id              :: String
    , keepMounted     :: Boolean
    , opened          :: Boolean
    , overlayProps    :: Record Props_OverlayInnerImpl
    , padding         :: MantineNumberSizeImpl
    , shadow          :: MantineShadowImpl
    , size            :: DimensionImpl
    , title           :: JSX
    , transitionProps :: ModalTransitionPropsImpl
    , zIndex          :: ZIndexImpl
    | ModalNonDefaultable rest
    )

type ModalTransitionProps     = MantineTransitionBase     (exitDuration :: Optional    Milliseconds)
type ModalTransitionPropsImpl = MantineTransitionBaseImpl (exitDuration :: UndefinedOr Number      )
