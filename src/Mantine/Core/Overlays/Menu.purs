module Mantine.Core.Overlays.Menu
  ( menu
  , menu_
  , MenuProps
  , MenuArrowPosition(..)
  , MenuFloatingPosition(..)
  , MenuPopoverWidth(..)
  , MenuTrigger(..)

  , menuItem
  , menuItem_
  , MenuItemProps

  , menuDropdown

  , menuTarget
  , menuTarget_
  , MenuTargetProps

  , menuLabel
  , menuDivider
  ) where

import Mantine.Core.Prelude
import React.Basic (element)
import React.Basic.DOM.Events (preventDefault)

menu :: (MenuProps -> MenuProps) -> JSX
menu = mkComponentWithDefault menuComponent defaultMenuProps

menu_ :: Array JSX -> JSX
menu_ children = menu _ { children = children }

foreign import menuComponent :: ReactComponent MenuPropsImpl

-- Not supported properties
--   { portalProps          :: Omit<PortalProps, "children">
--   , positionDependencies :: any[]
--   }

type MenuProps =
  MantineComponent
    ( arrowOffset         :: Optional Pixels
    , arrowPosition       :: Optional MenuArrowPosition
    , arrowRadius         :: Optional Pixels
    , arrowSize           :: Optional Pixels
    , children            :: Array JSX
    , clickOutsideEvents  :: Optional (Array String)
    , closeDelay          :: Optional Milliseconds
    , closeOnClickOutside :: Boolean
    , closeOnEscape       :: Boolean
    , closeOnItemClick    :: Boolean
    , defaultOpened       :: Optional Boolean
    , disabled            :: Boolean
    , id                  :: Optional String
    , keepMounted         :: Boolean
    , loop                :: Boolean
    , middlewares         :: PopoverMiddlewares
    , offset              :: Optional Pixels
    , onChange            :: ValueHandler Boolean
    , onClose             :: Effect Unit
    , onOpen              :: Effect Unit
    , onPositionChange    :: ValueHandler MenuFloatingPosition
    , openDelay           :: Optional Milliseconds
    , opened              :: Optional Boolean
    , position            :: MenuFloatingPosition
    , radius              :: Optional MantineNumberSize
    , returnFocus         :: Boolean
    , shadow              :: Optional MantineShadow
    , transitionProps     :: MantineTransitionProps
    , trigger             :: Optional MenuTrigger
    , width               :: Optional MenuPopoverWidth
    , withArrow           :: Boolean
    , withinPortal        :: Boolean
    , zIndex              :: Optional ZIndex
    )

defaultMenuProps :: MenuProps
defaultMenuProps =
  defaultMantineComponent
    { closeOnClickOutside: true
    , closeOnEscape:       true
    , closeOnItemClick:    true
    , onClose:             pure unit
    , onOpen:              pure unit
    , withinPortal:        true
    }

type MenuPropsImpl =
  MantineComponentImpl
    ( arrowOffset         :: OptionalImpl PixelsImpl
    , arrowPosition       :: OptionalImpl MenuArrowPositionImpl
    , arrowRadius         :: OptionalImpl PixelsImpl
    , arrowSize           :: OptionalImpl PixelsImpl
    , children            :: Array JSX
    , clickOutsideEvents  :: OptionalImpl (Array String)
    , closeDelay          :: OptionalImpl MillisecondsImpl
    , closeOnClickOutside :: Boolean
    , closeOnEscape       :: Boolean
    , closeOnItemClick    :: Boolean
    , defaultOpened       :: OptionalImpl Boolean
    , disabled            :: Boolean
    , id                  :: OptionalImpl String
    , keepMounted         :: Boolean
    , loop                :: Boolean
    , middlewares         :: PopoverMiddlewaresImpl
    , offset              :: OptionalImpl PixelsImpl
    , onChange            :: ValueHandlerImpl Boolean
    , onClose             :: Effect Unit
    , onOpen              :: Effect Unit
    , onPositionChange    :: ValueHandlerImpl MenuFloatingPositionImpl
    , openDelay           :: OptionalImpl MillisecondsImpl
    , opened              :: OptionalImpl Boolean
    , position            :: MenuFloatingPositionImpl
    , radius              :: OptionalImpl MantineNumberSizeImpl
    , returnFocus         :: Boolean
    , shadow              :: OptionalImpl MantineShadowImpl
    , transitionProps     :: MantineTransitionPropsImpl
    , trigger             :: OptionalImpl MenuTriggerImpl
    , width               :: OptionalImpl MenuPopoverWidthImpl
    , withArrow           :: Boolean
    , withinPortal        :: Boolean
    , zIndex              :: OptionalImpl ZIndexImpl
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

type MenuTriggerImpl = String

instance ToFFI MenuTrigger MenuTriggerImpl where
  toNative = case _ of
    MenuTriggerClick -> "click"
    MenuTriggerHover -> "hover"

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

instance DefaultValue MenuFloatingPosition where
  defaultValue = MenuFloatingPositionBottom

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
    _              -> defaultValue

data MenuArrowPosition
  = MenuArrowPositionCenter
  | MenuArrowPositionSide

type MenuArrowPositionImpl = String

instance ToFFI MenuArrowPosition MenuArrowPositionImpl where
  toNative = case _ of
    MenuArrowPositionCenter -> "center"
    MenuArrowPositionSide   -> "side"

-- ----------------------------------------------------------------------------

menuItem :: Effect Unit -> (MenuItemProps -> MenuItemProps) -> JSX
menuItem = mkComponent menuItemComponent menuItemToImpl <<< defaultMenuItemProps

menuItem_ :: Effect Unit -> JSX -> JSX
menuItem_ onClick children = (menuItem onClick) _ { children = pure children }

foreign import menuItemComponent :: ReactComponent MenuItemPropsImpl

type MenuItemProps =
  MantineComponent
    ( children         :: Array JSX
    , closeMenuOnClick :: Optional Boolean
    , color            :: Optional MantineColor
    , disabled         :: Boolean
    , leftSection      :: Optional JSX
    , onClick          :: Effect Unit
    , rightSection     :: Optional JSX
    )

defaultMenuItemProps :: Effect Unit -> MenuItemProps
defaultMenuItemProps onClick = defaultMantineComponent { onClick }

type MenuItemPropsImpl =
  MantineComponentImpl
    ( children         :: Array JSX
    , closeMenuOnClick :: OptionalImpl Boolean
    , color            :: OptionalImpl MantineColorImpl
    , disabled         :: Boolean
    , leftSection      :: OptionalImpl JSX
    , onClick          :: EventHandler
    , rightSection     :: OptionalImpl JSX
    )

menuItemToImpl :: MenuItemProps -> MenuItemPropsImpl
menuItemToImpl props =
  let rest = toNative
         <<< delete (Proxy :: Proxy "onClick")
   in { onClick: handler preventDefault (const props.onClick)
      } `union` rest props

menuDropdown :: Array JSX -> JSX
menuDropdown children = element menuDropdownComponent { children }

foreign import menuDropdownComponent :: ReactComponent { children :: Array JSX }

menuTarget :: (MenuTargetProps -> MenuTargetProps) -> JSX
menuTarget = mkTrivialComponent menuTargetComponent

menuTarget_ :: JSX -> JSX
menuTarget_ target = menuTarget _ { children = pure target }

foreign import menuTargetComponent :: ReactComponent MenuTargetPropsImpl

type MenuTargetProps =
  MantineComponent
    ( children :: Array JSX
    , refProp  :: Optional String
    )

type MenuTargetPropsImpl =
  MantineComponentImpl
    ( children :: Array JSX
    , refProp  :: OptionalImpl String
    )

menuLabel :: JSX -> JSX
menuLabel label = element menuLabelComponent { children: pure label }

foreign import menuLabelComponent :: ReactComponent { children :: Array JSX }

menuDivider :: JSX
menuDivider = element menuDividerComponent {}

foreign import menuDividerComponent :: ReactComponent {}
