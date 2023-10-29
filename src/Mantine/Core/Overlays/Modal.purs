module Mantine.Core.Overlays.Modal
  ( modal
  , modal_
  , ModalProps
  , ModalOverlayProps
  , ModalTransitionProps

  , ModalOverlayPropsImpl
  , ModalTransitionPropsImpl
  ) where

import Mantine.Core.Prelude

modal :: (ModalProps -> ModalProps) -> JSX
modal = mkComponentWithDefault modalComponent defaultModalProps

modal_ :: Array JSX -> JSX
modal_ children = modal _ { children = children }

foreign import modalComponent :: ReactComponent ModalPropsImpl

-- Not supported properties
--    { closeButtonProps :: ModalBaseCloseButtonProps
--    , target           :: string | HTMLElement
--    , xOffset          :: MarginLeft<string | number>
--    , yOffset          :: MarginTop<string | number>
--    }

type ModalProps =
  ThemingProps
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
    , overlayProps        :: ModalOverlayProps
    , padding             :: Maybe MantineNumberSize
    , radius              :: Maybe MantineNumberSize
    , returnFocus         :: Boolean
 -- , shadow              -- TODO
    , size                :: Maybe Dimension
    , title               :: Maybe JSX
    , transitionProps     :: ModalTransitionProps
    , trapFocus           :: Boolean
    , withCloseButton     :: Boolean
    , withOverlay         :: Boolean
    , withinPortal        :: Boolean
    , zIndex              :: Maybe Number
    )

type ModalOverlayProps =
  { blur    :: Maybe Number
  , color   :: Maybe String
  , opacity :: Maybe Number
  }

type ModalTransitionProps =
  { exitDuration   :: Maybe Milliseconds
  , transition     :: Maybe MantineTransition
  , duration       :: Maybe Milliseconds
  , timingFunction :: Maybe MantineTransitionTimingFunction
  }

defaultModalProps :: ModalProps
defaultModalProps =
  defaultThemingProps
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
  ThemingPropsImpl
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
    , overlayProps        :: ModalOverlayPropsImpl
    , padding             :: Nullable MantineNumberSizeImpl
    , radius              :: Nullable MantineNumberSizeImpl
    , returnFocus         :: Boolean
 -- , shadow              -- TODO
    , size                :: Nullable DimensionImpl
    , title               :: Nullable JSX
    , transitionProps     :: ModalTransitionPropsImpl
    , trapFocus           :: Boolean
    , withCloseButton     :: Boolean
    , withOverlay         :: Boolean
    , withinPortal        :: Boolean
    , zIndex              :: Nullable Number
    )

type ModalOverlayPropsImpl =
  { blur    :: Nullable Number
  , color   :: Nullable String
  , opacity :: Nullable Number
  }

type ModalTransitionPropsImpl =
  { exitDuration   :: Nullable Number
  , transition     :: Nullable String
  , duration       :: Nullable Number
  , timingFunction :: Nullable String
  }
