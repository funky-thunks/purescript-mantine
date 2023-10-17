module Mantine.Core.Overlays.Menu
  ( menu
  , menu_
  , MenuProps
  , MenuArrowPosition(..)
  , MenuFloatingPosition(..)
  , MenuPopoverWidth(..)
  , MenuTrigger(..)
  , PopoverMiddlewares

  , menuItem
  , menuItem_
  , MenuItemProps

  , menuDropdown
  , menuTarget
  , menuLabel
  , menuDivider
  ) where

import Prelude
import Mantine.Core.Prelude
import React.Basic (element)
import React.Basic.DOM.Events (preventDefault)

menu :: (MenuProps -> MenuProps) -> JSX
menu = mkComponent menuComponent menuToImpl defaultMenuProps

menu_ :: Array JSX -> JSX
menu_ children = menu _ { children = children }

foreign import menuComponent :: ReactComponent MenuPropsImpl

type MenuProps =
  ThemingProps
    ( arrowOffset            :: Maybe Pixels
    , arrowPosition          :: Maybe MenuArrowPosition
    , arrowRadius            :: Maybe Pixels
    , arrowSize              :: Maybe Pixels
    , children               :: Array JSX
    , clickOutsideEvents     :: Maybe (Array String)
    , closeDelay             :: Maybe Milliseconds
    , closeOnClickOutside    :: Boolean
    , closeOnEscape          :: Boolean
    , closeOnItemClick       :: Boolean
    , defaultOpened          :: Maybe Boolean
    , disabled               :: Boolean
    , exitTransitionDuration :: Maybe Milliseconds
    , id                     :: Maybe String
    , loop                   :: Boolean
    , middlewares            :: PopoverMiddlewares
    , offset                 :: Maybe Pixels
    , onChange               :: ValueHandler Boolean
    , onClose                :: Effect Unit
    , onOpen                 :: Effect Unit
    , onPositionChange       :: ValueHandler MenuFloatingPosition
    , openDelay              :: Maybe Milliseconds
    , opened                 :: Maybe Boolean
    , position               :: MenuFloatingPosition
    , radius                 :: Maybe MantineNumberSize
    , returnFocus            :: Boolean
    -- , shadow -- TODO
    , transition             :: Maybe MantineTransition
    , transitionDuration     :: Maybe Milliseconds
    , trigger                :: Maybe MenuTrigger
    , width                  :: Maybe MenuPopoverWidth
    , withArrow              :: Boolean
    , withinPortal           :: Boolean
    , zIndex                 :: Maybe Number
    )

defaultMenuProps :: MenuProps
defaultMenuProps =
  defaultThemingProps
    { closeOnClickOutside: true
    , closeOnEscape:       true
    , closeOnItemClick:    true
    , onClose:             pure unit
    , onOpen:              pure unit
    -- , shadow -- TODO
    } `union` defaultValue

type MenuPropsImpl =
  ThemingPropsImpl
    ( arrowOffset            :: Nullable Number
    , arrowPosition          :: Nullable String
    , arrowRadius            :: Nullable Number
    , arrowSize              :: Nullable Number
    , children               :: Array JSX
    , clickOutsideEvents     :: Nullable (Array String)
    , closeDelay             :: Nullable Number
    , closeOnClickOutside    :: Boolean
    , closeOnEscape          :: Boolean
    , closeOnItemClick       :: Boolean
    , defaultOpened          :: Nullable Boolean
    , disabled               :: Boolean
    , exitTransitionDuration :: Nullable Number
    , id                     :: Nullable String
    , loop                   :: Boolean
    , middlewares            :: PopoverMiddlewaresImpl
    , offset                 :: Nullable Number
    , onChange               :: EffectFn1 Boolean Unit
    , onClose                :: Effect Unit
    , onOpen                 :: Effect Unit
    , onPositionChange       :: EffectFn1 String Unit
    , openDelay              :: Nullable Number
    , opened                 :: Nullable Boolean
    , position               :: String
    , radius                 :: Nullable MantineNumberSizeImpl
    , returnFocus            :: Boolean
    -- , shadow -- TODO
    , transition             :: Nullable String
    , transitionDuration     :: Nullable Number
    , trigger                :: Nullable String
    , width                  :: Nullable String
    , withArrow              :: Boolean
    , withinPortal           :: Boolean
    , zIndex                 :: Nullable Number
    )

menuToImpl :: MenuProps -> MenuPropsImpl
menuToImpl =
  themingToImpl \ props@{ children, closeOnClickOutside, closeOnEscape, closeOnItemClick, disabled, loop, onClose, onOpen, returnFocus, withArrow, withinPortal } ->
    { children, closeOnClickOutside, closeOnEscape, closeOnItemClick, disabled, loop, onClose, onOpen, returnFocus, withArrow, withinPortal
    , middlewares: toNative props.middlewares

    , arrowOffset:            toNative props.arrowOffset
    , arrowPosition:          toNative props.arrowPosition
    , arrowRadius:            toNative props.arrowRadius
    , arrowSize:              toNative props.arrowSize
    , clickOutsideEvents:     toNative props.clickOutsideEvents
    , closeDelay:             toNative props.closeDelay
    , defaultOpened:          toNative props.defaultOpened
    , exitTransitionDuration: toNative props.exitTransitionDuration
    , id:                     toNative props.id
    , offset:                 toNative props.offset
    , openDelay:              toNative props.openDelay
    , opened:                 toNative props.opened
    , position:               toNative props.position
    , radius:                 toNative props.radius
    , transition:             toNative props.transition
    , transitionDuration:     toNative props.transitionDuration
    , trigger:                toNative props.trigger
    , width:                  toNative props.width
    , zIndex:                 toNative props.zIndex

    , onChange:               toNative props.onChange
    , onPositionChange:       toNative props.onPositionChange
    }

data MenuPopoverWidth
  = MenuPopoverWidthTarget
  | MenuPopoverWidthNative String

instance ToFFI MenuPopoverWidth String where
  toNative = case _ of
    MenuPopoverWidthTarget -> "target"
    MenuPopoverWidthNative n -> n

data MenuTrigger
  = MenuTriggerClick
  | MenuTriggerHover

instance ToFFI MenuTrigger String where
  toNative = case _ of
    MenuTriggerClick -> "click"
    MenuTriggerHover -> "hover"

type PopoverMiddlewares =
  { shift  :: Boolean
  , flip   :: Boolean
  , inline :: Maybe Boolean
  }

type PopoverMiddlewaresImpl =
  { shift  :: Boolean
  , flip   :: Boolean
  , inline :: Nullable Boolean
  }

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

instance DefaultValue MenuFloatingPosition where defaultValue = MenuFloatingPositionBottom

instance ToFFI MenuFloatingPosition String where
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

instance ToFFI MenuArrowPosition String where
  toNative = case _ of
    MenuArrowPositionCenter -> "center"
    MenuArrowPositionSide   -> "side"

-- ----------------------------------------------------------------------------

menuItem :: (MenuItemProps -> MenuItemProps) -> JSX
menuItem = mkComponent menuItemComponent menuItemToImpl defaultMenuItemProps

menuItem_ :: JSX -> JSX
menuItem_ children = menuItem _ { children = pure children }

foreign import menuItemComponent :: ReactComponent MenuItemPropsImpl

type MenuItemProps =
  ThemingProps
    ( children         :: Array JSX
    , closeMenuOnClick :: Maybe Boolean
    , color            :: Maybe MantineColor
    , icon             :: Maybe JSX
    , onClick          :: Effect Unit
    , rightSection     :: Maybe JSX
    )

defaultMenuItemProps :: MenuItemProps
defaultMenuItemProps =
  defaultThemingProps
    { onClick: pure unit
    } `union` defaultValue

type MenuItemPropsImpl =
  ThemingPropsImpl
    ( children         :: Array JSX
    , closeMenuOnClick :: Nullable Boolean
    , color            :: Nullable String
    , icon             :: Nullable JSX
    , onClick          :: EventHandler
    , rightSection     :: Nullable JSX
    )

menuItemToImpl :: MenuItemProps -> MenuItemPropsImpl
menuItemToImpl props =
  toNative (delete (Proxy :: Proxy "onClick") props)
    `union`
    { onClick: handler preventDefault (const props.onClick)
    }

menuDropdown :: Array JSX -> JSX
menuDropdown children = element menuDropdownComponent { children }

foreign import menuDropdownComponent :: ReactComponent { children :: Array JSX }

menuTarget :: JSX -> JSX
menuTarget target = element menuTargetComponent { children: pure target }

foreign import menuTargetComponent :: ReactComponent { children :: Array JSX }

menuLabel :: JSX -> JSX
menuLabel label = element menuLabelComponent { children: pure label }

foreign import menuLabelComponent :: ReactComponent { children :: Array JSX }

menuDivider :: JSX
menuDivider = element menuDividerComponent {}

foreign import menuDividerComponent :: ReactComponent {}
