module Mantine.Core.Overlays.Menu
  ( menu
  , menu_
  , Props_Menu
  , Props_MenuImpl
  , MenuArrowPosition(..)
  , MenuArrowPositionImpl
  , MenuFloatingPosition(..)
  , MenuFloatingPositionImpl
  , MenuPopoverWidth(..)
  , MenuPopoverWidthImpl
  , MenuTrigger(..)
  , MenuTriggerImpl

  , menuItem
  , menuItem_
  , Props_MenuItem
  , Props_MenuItemImpl
  , ClickHandler(..)
  , ClickHandlerImpl

  , menuDropdown

  , menuTarget
  , menuTarget_
  , Props_MenuTarget
  , Props_MenuTargetImpl

  , menuLabel
  , menuDivider
  ) where

import Mantine.Core.Prelude
import React.Basic (element)
import React.Basic.DOM.Events (preventDefault)

menu_ :: Array JSX -> JSX
menu_ children = menu { children }

foreign import menuComponent :: ReactComponent (Record Props_MenuImpl)

-- Not supported properties
--   { portalProps          :: Omit<PortalProps, "children">
--   , positionDependencies :: any[]
--   }

type Props_Menu =
  Props_Common
    ( arrowOffset         :: Pixels
    , arrowPosition       :: MenuArrowPosition
    , arrowRadius         :: Pixels
    , arrowSize           :: Pixels
    , children            :: Array JSX
    , clickOutsideEvents  :: (Array String)
    , closeDelay          :: Milliseconds
    , closeOnClickOutside :: Boolean
    , closeOnEscape       :: Boolean
    , closeOnItemClick    :: Boolean
    , defaultOpened       :: Boolean
    , disabled            :: Boolean
    , id                  :: String
    , keepMounted         :: Boolean
    , loop                :: Boolean
    , middlewares         :: PopoverMiddlewares
    , offset              :: Pixels
    , onChange            :: ValueHandler Boolean
    , onClose             :: Effect Unit
    , onOpen              :: Effect Unit
    , onPositionChange    :: ValueHandler MenuFloatingPosition
    , openDelay           :: Milliseconds
    , opened              :: Boolean
    , position            :: MenuFloatingPosition
    , radius              :: MantineNumberSize
    , returnFocus         :: Boolean
    , shadow              :: MantineShadow
    , transitionProps     :: MantineTransitionProps
    , trigger             :: MenuTrigger
    , width               :: MenuPopoverWidth
    , withArrow           :: Boolean
    , withinPortal        :: Boolean
    , zIndex              :: ZIndex
    )

type Props_MenuImpl =
  Props_CommonImpl
    ( arrowOffset         :: PixelsImpl
    , arrowPosition       :: MenuArrowPositionImpl
    , arrowRadius         :: PixelsImpl
    , arrowSize           :: PixelsImpl
    , children            :: Array JSX
    , clickOutsideEvents  :: (Array String)
    , closeDelay          :: MillisecondsImpl
    , closeOnClickOutside :: Boolean
    , closeOnEscape       :: Boolean
    , closeOnItemClick    :: Boolean
    , defaultOpened       :: Boolean
    , disabled            :: Boolean
    , id                  :: String
    , keepMounted         :: Boolean
    , loop                :: Boolean
    , middlewares         :: PopoverMiddlewaresImpl
    , offset              :: PixelsImpl
    , onChange            :: ValueHandlerImpl Boolean
    , onClose             :: Effect Unit
    , onOpen              :: Effect Unit
    , onPositionChange    :: ValueHandlerImpl MenuFloatingPositionImpl
    , openDelay           :: MillisecondsImpl
    , opened              :: Boolean
    , position            :: MenuFloatingPositionImpl
    , radius              :: MantineNumberSizeImpl
    , returnFocus         :: Boolean
    , shadow              :: MantineShadowImpl
    , transitionProps     :: MantineTransitionPropsImpl
    , trigger             :: MenuTriggerImpl
    , width               :: MenuPopoverWidthImpl
    , withArrow           :: Boolean
    , withinPortal        :: Boolean
    , zIndex              :: ZIndexImpl
    )

data MenuPopoverWidth
  = MenuPopoverWidthTarget
  | MenuPopoverWidthNative String

type MenuPopoverWidthImpl = String

instance ToFFI MenuPopoverWidth MenuPopoverWidthImpl where
  toNative = case _ of
    MenuPopoverWidthTarget -> "target"
    MenuPopoverWidthNative n -> n

data MenuTrigger
  = MenuTriggerClick
  | MenuTriggerHover
  | MenuTriggerClickHover

type MenuTriggerImpl = String

instance ToFFI MenuTrigger MenuTriggerImpl where
  toNative = case _ of
    MenuTriggerClick      -> "click"
    MenuTriggerHover      -> "hover"
    MenuTriggerClickHover -> "click-hover"

data MenuFloatingPosition
  = MenuFloatingPositionTop
  | MenuFloatingPositionRight
  | MenuFloatingPositionBottom
  | MenuFloatingPositionLeft
  | MenuFloatingPositionTopStart
  | MenuFloatingPositionRightStart
  | MenuFloatingPositionBottomStart
  | MenuFloatingPositionLeftStart
  | MenuFloatingPositionTopEnd
  | MenuFloatingPositionRightEnd
  | MenuFloatingPositionBottomEnd
  | MenuFloatingPositionLeftEnd

type MenuFloatingPositionImpl = String

instance ToFFI MenuFloatingPosition MenuFloatingPositionImpl where
  toNative = case _ of
    MenuFloatingPositionTop         -> "top"
    MenuFloatingPositionRight       -> "right"
    MenuFloatingPositionBottom      -> "bottom"
    MenuFloatingPositionLeft        -> "left"
    MenuFloatingPositionTopStart    -> "top-start"
    MenuFloatingPositionRightStart  -> "right-start"
    MenuFloatingPositionBottomStart -> "bottom-start"
    MenuFloatingPositionLeftStart   -> "left-start"
    MenuFloatingPositionTopEnd      -> "top-end"
    MenuFloatingPositionRightEnd    -> "right-end"
    MenuFloatingPositionBottomEnd   -> "bottom-end"
    MenuFloatingPositionLeftEnd     -> "left-end"

instance FromFFI String MenuFloatingPosition where
  fromNative = case _ of
    "top"          -> MenuFloatingPositionTop
    "right"        -> MenuFloatingPositionRight
    "bottom"       -> MenuFloatingPositionBottom
    "left"         -> MenuFloatingPositionLeft
    "top-start"    -> MenuFloatingPositionTopStart
    "right-start"  -> MenuFloatingPositionRightStart
    "bottom-start" -> MenuFloatingPositionBottomStart
    "left-start"   -> MenuFloatingPositionLeftStart
    "top-end"      -> MenuFloatingPositionTopEnd
    "right-end"    -> MenuFloatingPositionRightEnd
    "bottom-end"   -> MenuFloatingPositionBottomEnd
    "left-end"     -> MenuFloatingPositionLeftEnd
    _              -> MenuFloatingPositionBottom

data MenuArrowPosition
  = MenuArrowPositionCenter
  | MenuArrowPositionSide

type MenuArrowPositionImpl = String

instance ToFFI MenuArrowPosition MenuArrowPositionImpl where
  toNative = case _ of
    MenuArrowPositionCenter -> "center"
    MenuArrowPositionSide   -> "side"

-- ----------------------------------------------------------------------------

menuItem_ :: Effect Unit -> JSX -> JSX
menuItem_ onClick children =
  menuItem { children: pure children
           , onClick: ClickHandler onClick
           }

foreign import menuItemComponent :: ReactComponent (Record Props_MenuItemImpl)

type Props_MenuItem =
  Props_Common
    ( children         :: Array JSX
    , closeMenuOnClick :: Boolean
    , color            :: MantineColor
    , disabled         :: Boolean
    , leftSection      :: JSX
    , onClick          :: ClickHandler
    , rightSection     :: JSX
    )

type Props_MenuItemImpl =
  Props_CommonImpl
    ( children         :: Array JSX
    , closeMenuOnClick :: Boolean
    , color            :: MantineColorImpl
    , disabled         :: Boolean
    , leftSection      :: JSX
    , onClick          :: EventHandler
    , rightSection     :: JSX
    )

newtype ClickHandler = ClickHandler (Effect Unit)

type ClickHandlerImpl = EventHandler

instance ToFFI ClickHandler ClickHandlerImpl where
  toNative (ClickHandler h) = handler preventDefault (const h)

menuDropdown :: Array JSX -> JSX
menuDropdown children = element menuDropdownComponent { children }

foreign import menuDropdownComponent :: ReactComponent { children :: Array JSX }

menuTarget_ :: JSX -> JSX
menuTarget_ target = menuTarget { children: pure target }

foreign import menuTargetComponent :: ReactComponent (Record Props_MenuTargetImpl)

type Props_MenuTarget =
  Props_Common
    ( children :: Array JSX
    , refProp  :: String
    )

type Props_MenuTargetImpl =
  Props_CommonImpl
    ( children :: Array JSX
    , refProp  :: String
    )

menuLabel :: JSX -> JSX
menuLabel label = element menuLabelComponent { children: pure label }

foreign import menuLabelComponent :: ReactComponent { children :: Array JSX }

menuDivider :: JSX
menuDivider = element menuDividerComponent {}

foreign import menuDividerComponent :: ReactComponent {}

menu
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Menu
  => Union attrsImpl attrsImpl_ Props_MenuImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
menu = element (unsafeCoerce menuComponent) <<< toNative

menuItem
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_MenuItem
  => Union attrsImpl attrsImpl_ Props_MenuItemImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
menuItem = element (unsafeCoerce menuItemComponent) <<< toNative

menuTarget
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_MenuTarget
  => Union attrsImpl attrsImpl_ Props_MenuTargetImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
menuTarget = element (unsafeCoerce menuTargetComponent) <<< toNative
