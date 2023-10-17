module Mantine.Core.Overlays.Modal
  ( modal
  , modal_
  , ModalProps
  , ModalOverflow(..)
  ) where

import Prelude
import Mantine.Core.Prelude

modal :: (ModalProps -> ModalProps) -> JSX
modal = mkComponentWithDefault modalComponent defaultModalProps

modal_ :: Array JSX -> JSX
modal_ children = modal _ { children = children }

foreign import modalComponent :: ReactComponent ModalPropsImpl

type ModalProps =
  ThemingProps
    ( children                 :: Array JSX
    , centered                 :: Boolean
    , closeButtonLabel         :: Maybe String
    , closeOnClickOutside      :: Boolean
    , closeOnEscape            :: Boolean
    , exitTransitionDuration   :: Maybe Milliseconds
    , fullScreen               :: Boolean
    , id                       :: Maybe String
    , lockScroll               :: Boolean
    , onClose                  :: Effect Unit
    , opened                   :: Boolean
    , overflow                 :: ModalOverflow
    , overlayBlur              :: Maybe Number
    , overlayColor             :: Maybe String
    , overlayOpacity           :: Maybe Number
    , padding                  :: Maybe MantineNumberSize
    , radius                   :: Maybe MantineNumberSize
    -- , shadow -- TODO
    , size                     :: Maybe Dimension
    -- , target -- TODO
    , title                    :: Maybe JSX
    , transition               :: Maybe MantineTransition
    , transitionDuration       :: Maybe Milliseconds
    , transitionTimingFunction :: Maybe MantineTransitionTimingFunction
    , trapFocus                :: Boolean
    , withCloseButton          :: Boolean
    , withFocusReturn          :: Boolean
    , withinPortal             :: Boolean
    , zIndex                   :: Maybe Number
    )

defaultModalProps :: ModalProps
defaultModalProps =
  defaultThemingProps
    { children:                  []
    , centered:                  false
    , closeButtonLabel:          Nothing
    , closeOnClickOutside:       true
    , closeOnEscape:             true
    , exitTransitionDuration:    Nothing
    , fullScreen:                false
    , id:                        Nothing
    , lockScroll:                true
    , onClose:                   (pure unit)
    , opened:                    false
    , overflow:                  ModalOverflowInside
    , overlayBlur:               Nothing
    , overlayColor:              Nothing
    , overlayOpacity:            Nothing
    , padding:                   Nothing
    , radius:                    Nothing
    -- , shadow -- TODO
    , size:                      Nothing
    -- , target -- TODO
    , title:                     Nothing
    , transition:                Nothing
    , transitionDuration:        Nothing
    , transitionTimingFunction:  Nothing
    , trapFocus:                 false
    , withCloseButton:           true
    , withFocusReturn:           true
    , withinPortal:              true
    , zIndex:                    Nothing
    }

data ModalOverflow
  = ModalOverflowInside
  | ModalOverflowOutside

instance ToFFI ModalOverflow String where
  toNative = case _ of
    ModalOverflowInside  -> "inside"
    ModalOverflowOutside -> "outside"

type ModalPropsImpl =
  ThemingPropsImpl
    ( children                 :: Array JSX
    , centered                 :: Boolean
    , closeButtonLabel         :: Nullable String
    , closeOnClickOutside      :: Boolean
    , closeOnEscape            :: Boolean
    , exitTransitionDuration   :: Nullable Number
    , fullScreen               :: Boolean
    , id                       :: Nullable String
    , lockScroll               :: Boolean
    , onClose                  :: Effect Unit
    , opened                   :: Boolean
    , overflow                 :: String
    , overlayBlur              :: Nullable Number
    , overlayColor             :: Nullable String
    , overlayOpacity           :: Nullable Number
    , padding                  :: Nullable MantineNumberSizeImpl
    , radius                   :: Nullable MantineNumberSizeImpl
    -- , shadow -- TODO
    , size                     :: Nullable DimensionImpl
    -- , target -- TODO
    , title                    :: Nullable JSX
    , transition               :: Nullable String
    , transitionDuration       :: Nullable Number
    , transitionTimingFunction :: Nullable String
    , trapFocus                :: Boolean
    , withCloseButton          :: Boolean
    , withFocusReturn          :: Boolean
    , withinPortal             :: Boolean
    , zIndex                   :: Nullable Number
    )
