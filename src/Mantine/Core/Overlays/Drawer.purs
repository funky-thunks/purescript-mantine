module Mantine.Core.Overlays.Drawer
  ( drawer
  , DrawerProps
  , DrawerPosition(..)
  , module Mantine.Core.Overlays.Modal
  ) where

import Mantine.Core.Overlays.Modal (ModalOverlayProps, ModalTransitionProps)
import Mantine.Core.Overlays.Modal as M
import Mantine.Core.Prelude
import Web.HTML (HTMLElement)
import Mantine.Core.Buttons.CloseButton (CloseButtonProps, CloseButtonPropsImpl, closeButtonPropsToImpl)

drawer :: (DrawerProps -> DrawerProps) -> JSX
drawer = mkComponent drawerComponent drawerToImpl defaultDrawerProps

foreign import drawerComponent :: ReactComponent DrawerPropsImpl

type DrawerProps =
  ThemingProps
    ( children            :: Array JSX
    , closeButtonProps    :: Maybe CloseButtonProps
    , closeOnClickOutside :: Boolean
    , closeOnEscape       :: Boolean
    , id                  :: Maybe String
    , keepMounted         :: Boolean
    , lockScroll          :: Boolean
    , onClose             :: Effect Unit
    , opened              :: Boolean
    , overlayProps        :: ModalOverlayProps
    , padding             :: Maybe MantineNumberSize
    , position            :: DrawerPosition
    , returnFocus         :: Boolean
 -- , shadow              :: MantineShadow -- TODO
    , size                :: Maybe MantineNumberSize
    , target              :: Maybe (Either String HTMLElement)
    , title               :: Maybe JSX
    , transitionProps     :: ModalTransitionProps
    , trapFocus           :: Boolean
    , withCloseButton     :: Boolean
    , withOverlay         :: Boolean
    , withinPortal        :: Boolean
    , zIndex              :: Maybe Number
    )

defaultDrawerProps :: DrawerProps
defaultDrawerProps = defaultThemingProps { onClose: pure unit }

data DrawerPosition = Bottom | Left | Right | Top

instance DefaultValue DrawerPosition where defaultValue = Left

instance ToFFI DrawerPosition String where
  toNative = case _ of
    Bottom -> "bottom"
    Left   -> "left"
    Right  -> "right"
    Top    -> "top"

type DrawerPropsImpl =
  ThemingPropsImpl
    ( children            :: Array JSX
    , closeButtonProps    :: Nullable CloseButtonPropsImpl
    , closeOnClickOutside :: Boolean
    , closeOnEscape       :: Boolean
    , id                  :: Nullable String
    , keepMounted         :: Boolean
    , lockScroll          :: Boolean
    , onClose             :: Effect Unit
    , opened              :: Boolean
    , overlayProps        :: M.ModalOverlayPropsImpl
    , padding             :: Nullable MantineNumberSizeImpl
    , position            :: String
    , returnFocus         :: Boolean
 -- , shadow              :: MantineShadow -- TODO
    , size                :: Nullable MantineNumberSizeImpl
    , target              :: Nullable (String |+| HTMLElement)
    , title               :: Nullable JSX
    , transitionProps     :: M.ModalTransitionPropsImpl
    , trapFocus           :: Boolean
    , withCloseButton     :: Boolean
    , withOverlay         :: Boolean
    , withinPortal        :: Boolean
    , zIndex              :: Nullable Number
    )

drawerToImpl :: DrawerProps -> DrawerPropsImpl
drawerToImpl props =
  let rest = toNative <<< delete (Proxy :: Proxy "closeButtonProps")
      closeButtonProps = toNullable (closeButtonPropsToImpl <$> props.closeButtonProps)
   in { closeButtonProps } `union` rest props
