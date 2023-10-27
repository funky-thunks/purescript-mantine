module Mantine.Core.Overlays.Drawer
  ( drawer
  , DrawerProps
  , DrawerPosition(..)
  ) where

import Mantine.Core.Prelude
import Web.HTML (HTMLElement)

drawer :: (DrawerProps -> DrawerProps) -> JSX
drawer = mkComponentWithDefault drawerComponent defaultDrawerProps

foreign import drawerComponent :: ReactComponent DrawerPropsImpl

type DrawerProps =
  ThemingProps
    ( children :: Array JSX
    , closeButtonLabel         :: Maybe String
    , closeOnClickOutside      :: Boolean
    , closeOnEscape            :: Boolean
    , id                       :: Maybe String
    , lockScroll               :: Boolean
    , onClose                  :: Effect Unit
    , opened                   :: Boolean
    , overlayBlur              :: Maybe Number
    , overlayColor             :: Maybe String
    , overlayOpacity           :: Maybe Number
    , padding                  :: Maybe MantineNumberSize
    , position                 :: DrawerPosition
 -- , shadow                   :: MantineShadow -- TODO
    , size                     :: Maybe MantineNumberSize
    , target                   :: Maybe (Either String HTMLElement)
    , title                    :: Maybe JSX
    , transition               :: Maybe MantineTransition
    , transitionDuration       :: Maybe Number
    , transitionTimingFunction :: Maybe String
    , trapFocus                :: Boolean
    , withCloseButton          :: Boolean
    , withFocusReturn          :: Boolean
    , withOverlay              :: Boolean
    , withinPortal             :: Boolean
    , zIndex                   :: Maybe Number
    )

type DrawerPropsImpl =
  ThemingPropsImpl
    ( children                 :: Array JSX
    , closeButtonLabel         :: Nullable String
    , closeOnClickOutside      :: Boolean
    , closeOnEscape            :: Boolean
    , id                       :: Nullable String
    , lockScroll               :: Boolean
    , onClose                  :: Effect Unit
    , opened                   :: Boolean
    , overlayBlur              :: Nullable Number
    , overlayColor             :: Nullable String
    , overlayOpacity           :: Nullable Number
    , padding                  :: Nullable MantineNumberSizeImpl
    , position                 :: String
 -- , shadow                   :: MantineShadow -- TODO
    , size                     :: Nullable MantineNumberSizeImpl
    , target                   :: Nullable (String |+| HTMLElement)
    , title                    :: Nullable JSX
    , transition               :: Nullable String
    , transitionDuration       :: Nullable Number
    , transitionTimingFunction :: Nullable String
    , trapFocus                :: Boolean
    , withCloseButton          :: Boolean
    , withFocusReturn          :: Boolean
    , withOverlay              :: Boolean
    , withinPortal             :: Boolean
    , zIndex                   :: Nullable Number
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
