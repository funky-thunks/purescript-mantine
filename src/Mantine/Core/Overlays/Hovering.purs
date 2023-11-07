module Mantine.Core.Overlays.Hovering
  ( hoverCard
  , HoverCardProps
  , hoverCardTarget
  , hoverCardDropdown

  , popover
  , PopoverProps
  , popoverTarget
  , popoverDropdown

  , HoveringCommons
  , HoveringDropdownProps
  , HoveringTargetProps
  , HoverableArrowPosition(..)
  , HoverableComponent
  , HoverableComponentImpl
  , HoverableFloatingPosition(..)
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
defaultHoverCardProps = defaultThemingProps defaultHoveringProps

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

hoverCardDropdown :: (HoveringDropdownProps -> HoveringDropdownProps) -> JSX
hoverCardDropdown = mkTrivialComponent hoverCardDropdownComponent

foreign import hoverCardDropdownComponent :: ReactComponent HoveringDropdownPropsImpl

type HoveringDropdownProps =
  ThemingProps
    ( children :: Array JSX
    )

type HoveringDropdownPropsImpl =
  ThemingPropsImpl
    ( children :: Array JSX
    )

popover :: (PopoverProps -> PopoverProps) -> JSX
popover = mkComponentWithDefault popoverComponent defaultPopoverProps

foreign import popoverComponent :: ReactComponent PopoverPropsImpl

defaultPopoverProps :: PopoverProps
defaultPopoverProps = defaultThemingProps defaultHoveringProps

type NonDefaultableHoveringProps =
  { closeOnClickOutside :: Boolean
  , closeOnEscape       :: Boolean
  , onClose             :: Effect Unit
  , onOpen              :: Effect Unit
  , withRoles           :: Boolean
  , withinPortal        :: Boolean
  }

defaultHoveringProps :: NonDefaultableHoveringProps
defaultHoveringProps =
  { closeOnClickOutside: true
  , closeOnEscape:       true
  , onClose:             pure unit
  , onOpen:              pure unit
  , withRoles:           true
  , withinPortal:        true
  }

type PopoverProps     = HoveringCommons     ()
type PopoverPropsImpl = HoveringCommonsImpl ()

popoverTarget :: (HoveringTargetProps -> HoveringTargetProps) -> JSX
popoverTarget = mkHoveringComponent popoverTargetComponent

mkHoveringComponent :: ReactComponent HoveringTargetPropsImpl -> (HoveringTargetProps -> HoveringTargetProps) -> JSX
mkHoveringComponent cmpt = mkComponent cmpt hoveringTargetToImpl defaultThemingProps_

foreign import popoverTargetComponent :: ReactComponent HoveringTargetPropsImpl

popoverDropdown :: (HoveringDropdownProps -> HoveringDropdownProps) -> JSX
popoverDropdown = mkTrivialComponent popoverDropdownComponent

foreign import popoverDropdownComponent :: ReactComponent HoveringDropdownPropsImpl

type HoverableComponent rest =
  ThemingProps
    ( arrowOffset          :: Maybe Number
    , arrowPosition        :: Maybe HoverableArrowPosition
    , arrowRadius          :: Maybe Number
    , arrowSize            :: Maybe Number
    , children             :: Array JSX
    , disabled             :: Boolean
    , keepMounted          :: Boolean
    , offset               :: Maybe Number
    , onPositionChange     :: ValueHandler HoverableFloatingPosition
    , opened               :: Maybe Boolean
    , position             :: HoverableFloatingPosition
 -- , positionDependencies :: any[] -- TODO
    , radius               :: Maybe MantineNumberSize
    , transitionProps      :: ModalTransitionProps
    , withArrow            :: Boolean
    , withinPortal         :: Boolean
    , zIndex               :: Maybe Number
    | rest
    )

type HoveringCommons rest =
  HoverableComponent
    ( clickOutsideEvents  :: Array String
    , closeOnClickOutside :: Boolean
    , closeOnEscape       :: Boolean
    , defaultOpened       :: Maybe Boolean
    , id                  :: Maybe String
 -- , middlewares         :: PopoverMiddlewares -- TODO
    , onChange            :: ValueHandler Boolean
    , onClose             :: Effect Unit
    , onOpen              :: Effect Unit
    , returnFocus         :: Boolean
    , shadow              :: Maybe MantineShadow
    , trapFocus           :: Boolean
    , width               :: HoverPopoverWidth
    , withRoles           :: Boolean
    | rest
    )

data HoverableFloatingPosition
  = HoverableFloatingPositionTop
  | HoverableFloatingPositionRight
  | HoverableFloatingPositionBottom
  | HoverableFloatingPositionLeft
  | HoverableFloatingPositionTopStart
  | HoverableFloatingPositionRightStart
  | HoverableFloatingPositionBottomStart
  | HoverableFloatingPositionLeftStart
  | HoverableFloatingPositionTopEnd
  | HoverableFloatingPositionRightEnd
  | HoverableFloatingPositionBottomEnd
  | HoverableFloatingPositionLeftEnd

instance DefaultValue HoverableFloatingPosition where defaultValue = HoverableFloatingPositionBottom

instance ToFFI HoverableFloatingPosition String where
  toNative = case _ of
    HoverableFloatingPositionTop         -> "top"
    HoverableFloatingPositionRight       -> "right"
    HoverableFloatingPositionBottom      -> "bottom"
    HoverableFloatingPositionLeft        -> "left"
    HoverableFloatingPositionTopStart    -> "top-start"
    HoverableFloatingPositionRightStart  -> "right-start"
    HoverableFloatingPositionBottomStart -> "bottom-start"
    HoverableFloatingPositionLeftStart   -> "left-start"
    HoverableFloatingPositionTopEnd      -> "top-end"
    HoverableFloatingPositionRightEnd    -> "right-end"
    HoverableFloatingPositionBottomEnd   -> "bottom-end"
    HoverableFloatingPositionLeftEnd     -> "left-end"

instance FromFFI String HoverableFloatingPosition where
  fromNative = case _ of
    "top"          -> HoverableFloatingPositionTop
    "right"        -> HoverableFloatingPositionRight
    "bottom"       -> HoverableFloatingPositionBottom
    "left"         -> HoverableFloatingPositionLeft
    "top-end"      -> HoverableFloatingPositionTopEnd
    "right-end"    -> HoverableFloatingPositionRightEnd
    "bottom-end"   -> HoverableFloatingPositionBottomEnd
    "left-end"     -> HoverableFloatingPositionLeftEnd
    "top-start"    -> HoverableFloatingPositionTopStart
    "right-start"  -> HoverableFloatingPositionRightStart
    "bottom-start" -> HoverableFloatingPositionBottomStart
    "left-start"   -> HoverableFloatingPositionLeftStart
    _              -> defaultValue

data HoverPopoverWidth = AsTarget | Fixed Number

instance DefaultValue HoverPopoverWidth where defaultValue = AsTarget

type HoverPopoverWidthImpl = String |+| Number

instance ToFFI HoverPopoverWidth HoverPopoverWidthImpl where
  toNative = case _ of
    AsTarget -> asOneOf "target"
    Fixed n  -> asOneOf n

type HoverableComponentImpl rest =
  ThemingPropsImpl
    ( arrowOffset          :: Nullable Number
    , arrowPosition        :: Nullable String
    , arrowRadius          :: Nullable Number
    , arrowSize            :: Nullable Number
    , children             :: Array JSX
    , disabled             :: Boolean
    , keepMounted          :: Boolean
    , offset               :: Nullable Number
    , onPositionChange     :: EffectFn1 String Unit
    , opened               :: Nullable Boolean
    , position             :: String
 -- , positionDependencies :: any[] -- TODO
    , radius               :: Nullable MantineNumberSizeImpl
    , transitionProps      :: ModalTransitionPropsImpl
    , withArrow            :: Boolean
    , withinPortal         :: Boolean
    , zIndex               :: Nullable Number
    | rest
    )

type HoveringCommonsImpl rest =
  HoverableComponentImpl
    ( clickOutsideEvents  :: Array String
    , closeOnClickOutside :: Boolean
    , closeOnEscape       :: Boolean
    , defaultOpened       :: Nullable Boolean
    , id                  :: Nullable String
 -- , middlewares         :: PopoverMiddlewares -- TODO
    , onChange            :: EffectFn1 Boolean Unit
    , onClose             :: Effect Unit
    , onOpen              :: Effect Unit
    , returnFocus         :: Boolean
    , shadow              :: Nullable String
    , trapFocus           :: Boolean
    , width               :: HoverPopoverWidthImpl
    , withRoles           :: Boolean
    | rest
    )

data HoverableArrowPosition
  = HoverableArrowPositionCenter
  | HoverableArrowPositionSide

instance ToFFI HoverableArrowPosition String where
  toNative = case _ of
    HoverableArrowPositionCenter -> "center"
    HoverableArrowPositionSide   -> "side"

type HoveringTargetProps =
  ThemingProps
    ( children              :: Array JSX
    , eventPropsWrapperName :: Maybe String
    , popupType             :: HoverPopupType
    , refProp               :: Maybe String
    )

data HoverPopupType = HoverPopupTypeDialog | HoverPopupTypeCustom String

instance DefaultValue HoverPopupType where defaultValue = HoverPopupTypeDialog

instance ToFFI HoverPopupType String where
  toNative = case _ of
    HoverPopupTypeDialog   -> "dialog"
    HoverPopupTypeCustom s -> s

type HoveringTargetPropsImpl =
  ThemingPropsImpl
    ( children              :: Array JSX
    , eventPropsWrapperName :: Nullable String
    , popupType             :: String
    , refProp               :: Nullable String
    )

hoveringTargetToImpl :: HoveringTargetProps -> HoveringTargetPropsImpl
hoveringTargetToImpl =
  let wrapChildren props = props { children = [ DOM.div_ props.children ] }
   in toNative >>> wrapChildren
