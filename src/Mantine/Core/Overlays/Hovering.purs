module Mantine.Core.Overlays.Hovering
  ( hoverCard
  , HoverCardProps
  , hoverCardTarget
  , hoverCardDropdown
  , HoverCardDropdownProps

  , popover
  , PopoverProps
  , popoverTarget
  , popoverDropdown
  , PopoverDropdownProps

  , HoveringCommons
  , HoveringTargetProps
  , HoverArrowPosition(..)
  , HoverFloatingPosition(..)
  , HoverPopoverWidth(..)
  , HoverPopupType(..)
  ) where

import Mantine.Core.Overlays.Modal (ModalTransitionProps, ModalTransitionPropsImpl)
import Mantine.Core.Prelude
import React.Basic.DOM as DOM

hoverCard :: (HoverCardProps -> HoverCardProps) -> JSX
hoverCard = mkComponentWithDefault hoverCardComponent defaultHoverCardProps

foreign import hoverCardComponent :: ReactComponent HoverCardPropsImpl

defaultHoverCardProps :: HoverCardProps
defaultHoverCardProps =
  defaultThemingProps
    { onClose: pure unit
    , onOpen:  pure unit
    }

type HoverCardProps =
  HoveringCommons
    ( closeDelay      :: Maybe Number
    , initiallyOpened :: Maybe Boolean
    , openDelay       :: Maybe Number
    )

type HoverCardPropsImpl =
  HoveringCommonsImpl
    ( closeDelay      :: Nullable Number
    , initiallyOpened :: Nullable Boolean
    , openDelay       :: Nullable Number
    )

hoverCardTarget :: (HoveringTargetProps -> HoveringTargetProps) -> JSX
hoverCardTarget = mkHoveringComponent hoverCardTargetComponent

foreign import hoverCardTargetComponent :: ReactComponent HoveringTargetPropsImpl

hoverCardDropdown :: (HoverCardDropdownProps -> HoverCardDropdownProps) -> JSX
hoverCardDropdown = mkTrivialComponent hoverCardDropdownComponent

foreign import hoverCardDropdownComponent :: ReactComponent HoverCardDropdownPropsImpl

type HoverCardDropdownProps =
  ThemingProps
    ( children :: Array JSX
    )

type HoverCardDropdownPropsImpl =
  ThemingPropsImpl
    ( children :: Array JSX
    )

popover :: (PopoverProps -> PopoverProps) -> JSX
popover = mkComponentWithDefault popoverComponent defaultPopoverProps

foreign import popoverComponent :: ReactComponent PopoverPropsImpl

defaultPopoverProps :: PopoverProps
defaultPopoverProps =
  defaultThemingProps
    { closeOnClickOutside: true
    , closeOnEscape:       true
    , onClose:             pure unit
    , onOpen:              pure unit
    , withRoles:           true
    }

type PopoverProps =
  HoveringCommons
    ( clickOutsideEvents  :: Array String
    , closeOnClickOutside :: Boolean
    , closeOnEscape       :: Boolean
    , defaultOpened       :: Maybe Boolean
    , id                  :: Maybe String
    , onChange            :: ValueHandler Boolean
    , opened              :: Maybe Boolean
    , trapFocus           :: Boolean
    , withRoles           :: Boolean
    )

type PopoverPropsImpl =
  HoveringCommonsImpl
    ( clickOutsideEvents  :: Array String
    , closeOnClickOutside :: Boolean
    , closeOnEscape       :: Boolean
    , defaultOpened       :: Nullable Boolean
    , id                  :: Nullable String
    , onChange            :: EffectFn1 Boolean Unit
    , opened              :: Nullable Boolean
    , trapFocus           :: Boolean
    , withRoles           :: Boolean
    )

popoverTarget :: (HoveringTargetProps -> HoveringTargetProps) -> JSX
popoverTarget = mkHoveringComponent popoverTargetComponent

mkHoveringComponent :: ReactComponent HoveringTargetPropsImpl -> (HoveringTargetProps -> HoveringTargetProps) -> JSX
mkHoveringComponent cmpt = mkComponent cmpt hoveringTargetToImpl defaultThemingProps_

foreign import popoverTargetComponent :: ReactComponent HoveringTargetPropsImpl

popoverDropdown :: (PopoverDropdownProps -> PopoverDropdownProps) -> JSX
popoverDropdown = mkTrivialComponent popoverDropdownComponent

foreign import popoverDropdownComponent :: ReactComponent PopoverDropdownPropsImpl

type PopoverDropdownProps =
  ThemingProps
    ( children :: Array JSX
    )

type PopoverDropdownPropsImpl =
  ThemingPropsImpl
    ( children :: Array JSX
    )

type HoveringCommons rest =
  ThemingProps
    ( children             :: Array JSX
    , arrowOffset          :: Maybe Number
    , arrowPosition        :: Maybe HoverArrowPosition
    , arrowRadius          :: Maybe Number
    , arrowSize            :: Maybe Number
    , disabled             :: Boolean
    , keepMounted          :: Boolean
 -- , middlewares          :: PopoverMiddlewares -- TODO
    , offset               :: Maybe Number
    , onClose              :: Effect Unit
    , onOpen               :: Effect Unit
    , onPositionChange     :: ValueHandler HoverFloatingPosition
    , position             :: HoverFloatingPosition
 -- , positionDependencies :: any[] -- TODO
    , radius               :: Maybe MantineNumberSize
    , returnFocus          :: Boolean
    , shadow               :: Maybe MantineShadow
    , transitionProps      :: ModalTransitionProps
    , width                :: HoverPopoverWidth
    , withArrow            :: Boolean
    , withinPortal         :: Boolean
    , zIndex               :: Maybe Number
    | rest
    )

data HoverFloatingPosition
  = HoverFloatingPositionTop      | HoverFloatingPositionRight      | HoverFloatingPositionBottom      | HoverFloatingPositionLeft
  | HoverFloatingPositionTopEnd   | HoverFloatingPositionRightEnd   | HoverFloatingPositionBottomEnd   | HoverFloatingPositionLeftEnd
  | HoverFloatingPositionTopStart | HoverFloatingPositionRightStart | HoverFloatingPositionBottomStart | HoverFloatingPositionLeftStart

instance DefaultValue HoverFloatingPosition where defaultValue = HoverFloatingPositionBottom

instance ToFFI HoverFloatingPosition String where
  toNative = case _ of
    HoverFloatingPositionTop         -> "top"
    HoverFloatingPositionRight       -> "right"
    HoverFloatingPositionBottom      -> "bottom"
    HoverFloatingPositionLeft        -> "left"
    HoverFloatingPositionTopEnd      -> "top-end"
    HoverFloatingPositionRightEnd    -> "right-end"
    HoverFloatingPositionBottomEnd   -> "bottom-end"
    HoverFloatingPositionLeftEnd     -> "left-end"
    HoverFloatingPositionTopStart    -> "top-start"
    HoverFloatingPositionRightStart  -> "right-start"
    HoverFloatingPositionBottomStart -> "bottom-start"
    HoverFloatingPositionLeftStart   -> "left-start"

instance FromFFI String HoverFloatingPosition where
  fromNative = case _ of
    "top"          -> HoverFloatingPositionTop
    "right"        -> HoverFloatingPositionRight
    "bottom"       -> HoverFloatingPositionBottom
    "left"         -> HoverFloatingPositionLeft
    "top-end"      -> HoverFloatingPositionTopEnd
    "right-end"    -> HoverFloatingPositionRightEnd
    "bottom-end"   -> HoverFloatingPositionBottomEnd
    "left-end"     -> HoverFloatingPositionLeftEnd
    "top-start"    -> HoverFloatingPositionTopStart
    "right-start"  -> HoverFloatingPositionRightStart
    "bottom-start" -> HoverFloatingPositionBottomStart
    "left-start"   -> HoverFloatingPositionLeftStart
    _              -> defaultValue

data HoverPopoverWidth = AsTarget | Fixed Number

instance DefaultValue HoverPopoverWidth where defaultValue = AsTarget

type HoverPopoverWidthImpl = String |+| Number

instance ToFFI HoverPopoverWidth HoverPopoverWidthImpl where
  toNative = case _ of
    AsTarget -> asOneOf "target"
    Fixed n  -> asOneOf n

type HoveringCommonsImpl rest =
  ThemingPropsImpl
    ( children             :: Array JSX
    , arrowOffset          :: Nullable Number
    , arrowPosition        :: Nullable String
    , arrowRadius          :: Nullable Number
    , arrowSize            :: Nullable Number
    , disabled             :: Boolean
    , keepMounted          :: Boolean
 -- , middlewares          :: PopoverMiddlewares -- TODO
    , offset               :: Nullable Number
    , onClose              :: Effect Unit
    , onOpen               :: Effect Unit
    , onPositionChange     :: EffectFn1 String Unit
    , position             :: String
 -- , positionDependencies :: any[] -- TODO
    , radius               :: Nullable MantineNumberSizeImpl
    , returnFocus          :: Boolean
    , shadow               :: Nullable String
    , transitionProps      :: ModalTransitionPropsImpl
    , width                :: HoverPopoverWidthImpl
    , withArrow            :: Boolean
    , withinPortal         :: Boolean
    , zIndex               :: Nullable Number
    | rest
    )

data HoverArrowPosition = HoverArrowPositionCenter | HoverArrowPositionSide

instance ToFFI HoverArrowPosition String where
  toNative = case _ of
    HoverArrowPositionCenter -> "center"
    HoverArrowPositionSide   -> "side"

type HoveringTargetProps =
  ThemingProps
    ( children                      :: Array JSX
    , popupType                     :: HoverPopupType
    , refProp                       :: Maybe String
    , shouldOverrideDefaultTargetId :: Boolean
    )

data HoverPopupType = HoverPopupTypeDialog | HoverPopupTypeCustom String

instance DefaultValue HoverPopupType where defaultValue = HoverPopupTypeDialog

instance ToFFI HoverPopupType String where
  toNative = case _ of
    HoverPopupTypeDialog   -> "dialog"
    HoverPopupTypeCustom s -> s

type HoveringTargetPropsImpl =
  ThemingPropsImpl
    ( children                      :: Array JSX
    , popupType                     :: String
    , refProp                       :: Nullable String
    , shouldOverrideDefaultTargetId :: Boolean
    )

hoveringTargetToImpl :: HoveringTargetProps -> HoveringTargetPropsImpl
hoveringTargetToImpl =
  let wrapChildren props = props { children = [ DOM.div_ props.children ] }
   in toNative >>> wrapChildren
