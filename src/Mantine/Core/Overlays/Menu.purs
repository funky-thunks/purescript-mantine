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
    ( arrowOffset         :: Maybe Pixels
    , arrowPosition       :: Maybe MenuArrowPosition
    , arrowRadius         :: Maybe Pixels
    , arrowSize           :: Maybe Pixels
    , children            :: Array JSX
    , clickOutsideEvents  :: Maybe (Array String)
    , closeDelay          :: Maybe Milliseconds
    , closeOnClickOutside :: Boolean
    , closeOnEscape       :: Boolean
    , closeOnItemClick    :: Boolean
    , defaultOpened       :: Maybe Boolean
    , disabled            :: Boolean
    , id                  :: Maybe String
    , keepMounted         :: Boolean
    , loop                :: Boolean
    , middlewares         :: PopoverMiddlewares
    , offset              :: Maybe Pixels
    , onChange            :: ValueHandler Boolean
    , onClose             :: Effect Unit
    , onOpen              :: Effect Unit
    , onPositionChange    :: ValueHandler MenuFloatingPosition
    , openDelay           :: Maybe Milliseconds
    , opened              :: Maybe Boolean
    , position            :: MenuFloatingPosition
    , radius              :: Maybe MantineNumberSize
    , returnFocus         :: Boolean
    , shadow              :: Maybe MantineShadow
    , transitionProps     :: MantineTransitionProps
    , trigger             :: Maybe MenuTrigger
    , width               :: Maybe MenuPopoverWidth
    , withArrow           :: Boolean
    , withinPortal        :: Boolean
    , zIndex              :: Maybe ZIndex
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
    ( arrowOffset         :: Nullable PixelsImpl
    , arrowPosition       :: Nullable MenuArrowPositionImpl
    , arrowRadius         :: Nullable PixelsImpl
    , arrowSize           :: Nullable PixelsImpl
    , children            :: Array JSX
    , clickOutsideEvents  :: Nullable (Array String)
    , closeDelay          :: Nullable MillisecondsImpl
    , closeOnClickOutside :: Boolean
    , closeOnEscape       :: Boolean
    , closeOnItemClick    :: Boolean
    , defaultOpened       :: Nullable Boolean
    , disabled            :: Boolean
    , id                  :: Nullable String
    , keepMounted         :: Boolean
    , loop                :: Boolean
    , middlewares         :: PopoverMiddlewaresImpl
    , offset              :: Nullable PixelsImpl
    , onChange            :: ValueHandlerImpl Boolean
    , onClose             :: Effect Unit
    , onOpen              :: Effect Unit
    , onPositionChange    :: ValueHandlerImpl MenuFloatingPositionImpl
    , openDelay           :: Nullable MillisecondsImpl
    , opened              :: Nullable Boolean
    , position            :: MenuFloatingPositionImpl
    , radius              :: Nullable MantineNumberSizeImpl
    , returnFocus         :: Boolean
    , shadow              :: Nullable MantineShadowImpl
    , transitionProps     :: MantineTransitionPropsImpl
    , trigger             :: Nullable MenuTriggerImpl
    , width               :: Nullable MenuPopoverWidthImpl
    , withArrow           :: Boolean
    , withinPortal        :: Boolean
    , zIndex              :: Nullable ZIndexImpl
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
    , closeMenuOnClick :: Maybe Boolean
    , color            :: Maybe MantineColor
    , disabled         :: Boolean
    , leftSection      :: Maybe JSX
    , onClick          :: Effect Unit
    , rightSection     :: Maybe JSX
    )

defaultMenuItemProps :: Effect Unit -> MenuItemProps
defaultMenuItemProps onClick = defaultMantineComponent { onClick }

type MenuItemPropsImpl =
  MantineComponentImpl
    ( children         :: Array JSX
    , closeMenuOnClick :: Nullable Boolean
    , color            :: Nullable MantineColorImpl
    , disabled         :: Boolean
    , leftSection      :: Nullable JSX
    , onClick          :: EventHandler
    , rightSection     :: Nullable JSX
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
    , refProp  :: Maybe String
    )

type MenuTargetPropsImpl =
  MantineComponentImpl
    ( children :: Array JSX
    , refProp  :: Nullable String
    )

menuLabel :: JSX -> JSX
menuLabel label = element menuLabelComponent { children: pure label }

foreign import menuLabelComponent :: ReactComponent { children :: Array JSX }

menuDivider :: JSX
menuDivider = element menuDividerComponent {}

foreign import menuDividerComponent :: ReactComponent {}
