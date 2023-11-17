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
  , HoveringCommonsImpl
  , HoveringDropdownProps
  , HoveringTarget
  , HoverableArrowPosition(..)
  , HoverableArrowPositionImpl
  , HoverableComponent
  , HoverableComponentImpl
  , HoverableFloatingPosition(..)
  , HoverableFloatingPositionImpl
  , HoverPopoverWidth(..)
  , HoverPopoverWidthImpl
  , HoverPopupType(..)
  , PopoverPropsImpl
  ) where

import Mantine.Core.Overlays.Modal (ModalTransitionProps, ModalTransitionPropsImpl)
import Mantine.Core.Prelude
import React.Basic.DOM as DOM
import Mantine.FFI (class RecordToFFI)
import Prim.RowList (class RowToList)

hoverCard :: (HoverCardProps -> HoverCardProps) -> JSX
hoverCard = mkComponentWithDefault hoverCardComponent defaultHoverCardProps

foreign import hoverCardComponent :: ReactComponent HoverCardPropsImpl

defaultHoverCardProps :: HoverCardProps
defaultHoverCardProps = defaultMantineComponent defaultHoveringProps

type HoverCardProps =
  HoveringCommons
    ( closeDelay      :: Optional Milliseconds
    , initiallyOpened :: Optional Boolean
    , openDelay       :: Optional Milliseconds
    )

type HoverCardPropsImpl =
  HoveringCommonsImpl
    ( closeDelay      :: OptionalImpl MillisecondsImpl
    , initiallyOpened :: OptionalImpl Boolean
    , openDelay       :: OptionalImpl MillisecondsImpl
    )

hoverCardTarget :: (HoverTargetProps -> HoverTargetProps) -> JSX
hoverCardTarget = mkComponent hoverCardTargetComponent hoveringTargetToImpl defaultMantineComponent_

foreign import hoverCardTargetComponent :: ReactComponent HoverTargetPropsImpl

type HoverTargetProps     = HoveringTarget     (eventPropsWrapperName :: Optional     String)
type HoverTargetPropsImpl = HoveringTargetImpl (eventPropsWrapperName :: OptionalImpl String)

hoverCardDropdown :: (HoveringDropdownProps -> HoveringDropdownProps) -> JSX
hoverCardDropdown = mkTrivialComponent hoverCardDropdownComponent

foreign import hoverCardDropdownComponent :: ReactComponent HoveringDropdownPropsImpl

popover :: (PopoverProps -> PopoverProps) -> JSX
popover = mkComponentWithDefault popoverComponent defaultPopoverProps

foreign import popoverComponent :: ReactComponent PopoverPropsImpl

defaultPopoverProps :: PopoverProps
defaultPopoverProps = defaultMantineComponent defaultHoveringProps

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

popoverTarget :: (PopoverTargetProps -> PopoverTargetProps) -> JSX
popoverTarget = mkComponent popoverTargetComponent hoveringTargetToImpl defaultMantineComponent_

type PopoverTargetProps     = HoveringTarget     ()
type PopoverTargetPropsImpl = HoveringTargetImpl ()

foreign import popoverTargetComponent :: ReactComponent PopoverTargetPropsImpl

popoverDropdown :: (HoveringDropdownProps -> HoveringDropdownProps) -> JSX
popoverDropdown = mkTrivialComponent popoverDropdownComponent

foreign import popoverDropdownComponent :: ReactComponent HoveringDropdownPropsImpl

type HoverableComponent rest =
  MantineComponent
    ( arrowOffset          :: Optional Number
    , arrowPosition        :: Optional HoverableArrowPosition
    , arrowRadius          :: Optional Number
    , arrowSize            :: Optional Number
    , children             :: Array JSX
    , disabled             :: Boolean
    , keepMounted          :: Boolean
    , offset               :: Optional Number
    , onPositionChange     :: ValueHandler HoverableFloatingPosition
    , opened               :: Optional Boolean
    , position             :: HoverableFloatingPosition
 -- , positionDependencies :: any[] -- TODO
    , radius               :: Optional MantineNumberSize
    , transitionProps      :: ModalTransitionProps
    , withArrow            :: Boolean
    , withinPortal         :: Boolean
    , zIndex               :: Optional ZIndex
    | rest
    )

type HoveringCommons rest =
  HoverableComponent
    ( clickOutsideEvents  :: Array String
    , closeOnClickOutside :: Boolean
    , closeOnEscape       :: Boolean
    , defaultOpened       :: Optional Boolean
    , id                  :: Optional String
 -- , middlewares         :: PopoverMiddlewares -- TODO
    , onChange            :: ValueHandler Boolean
    , onClose             :: Effect Unit
    , onOpen              :: Effect Unit
    , returnFocus         :: Boolean
    , shadow              :: Optional MantineShadow
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

instance DefaultValue HoverableFloatingPosition where
  defaultValue = HoverableFloatingPositionBottom

type HoverableFloatingPositionImpl = String

instance ToFFI HoverableFloatingPosition HoverableFloatingPositionImpl where
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
  MantineComponentImpl
    ( arrowOffset          :: OptionalImpl Number
    , arrowPosition        :: OptionalImpl HoverableArrowPositionImpl
    , arrowRadius          :: OptionalImpl Number
    , arrowSize            :: OptionalImpl Number
    , children             :: Array JSX
    , disabled             :: Boolean
    , keepMounted          :: Boolean
    , offset               :: OptionalImpl Number
    , onPositionChange     :: ValueHandlerImpl HoverableFloatingPositionImpl
    , opened               :: OptionalImpl Boolean
    , position             :: HoverableFloatingPositionImpl
 -- , positionDependencies :: any[] -- TODO
    , radius               :: OptionalImpl MantineNumberSizeImpl
    , transitionProps      :: ModalTransitionPropsImpl
    , withArrow            :: Boolean
    , withinPortal         :: Boolean
    , zIndex               :: OptionalImpl ZIndexImpl
    | rest
    )

type HoveringCommonsImpl rest =
  HoverableComponentImpl
    ( clickOutsideEvents  :: Array String
    , closeOnClickOutside :: Boolean
    , closeOnEscape       :: Boolean
    , defaultOpened       :: OptionalImpl Boolean
    , id                  :: OptionalImpl String
 -- , middlewares         :: PopoverMiddlewares -- TODO
    , onChange            :: ValueHandlerImpl Boolean
    , onClose             :: Effect Unit
    , onOpen              :: Effect Unit
    , returnFocus         :: Boolean
    , shadow              :: OptionalImpl MantineShadowImpl
    , trapFocus           :: Boolean
    , width               :: HoverPopoverWidthImpl
    , withRoles           :: Boolean
    | rest
    )

data HoverableArrowPosition
  = HoverableArrowPositionCenter
  | HoverableArrowPositionSide

type HoverableArrowPositionImpl = String

instance ToFFI HoverableArrowPosition HoverableArrowPositionImpl where
  toNative = case _ of
    HoverableArrowPositionCenter -> "center"
    HoverableArrowPositionSide   -> "side"

type HoveringTarget rest =
  MantineComponent
    ( children  :: Array JSX
    , popupType :: HoverPopupType
    , refProp   :: Optional String
    | rest
    )

data HoverPopupType
  = HoverPopupTypeDialog
  | HoverPopupTypeCustom String

instance DefaultValue HoverPopupType where defaultValue = HoverPopupTypeDialog

type HoverPopupTypeImpl = String

instance ToFFI HoverPopupType HoverPopupTypeImpl where
  toNative = case _ of
    HoverPopupTypeDialog   -> "dialog"
    HoverPopupTypeCustom s -> s

type HoveringTargetImpl rest =
  MantineComponentImpl
    ( children  :: Array JSX
    , popupType :: HoverPopupTypeImpl
    , refProp   :: OptionalImpl String
    | rest
    )

hoveringTargetToImpl :: forall rest restImpl others
                      . ToFFI (Record rest) (Record restImpl)
                     => RecordToFFI others (children :: Array JSX | rest) (children :: Array JSX | restImpl)
                     => RowToList ( children :: Array JSX | rest) others
                     => { children :: Array JSX | rest } -> { children :: Array JSX | restImpl }
hoveringTargetToImpl =
  let wrapChildren props = props { children = [ DOM.div_ props.children ] }
   in toNative >>> wrapChildren

type HoveringDropdownProps     = MantineComponent     (children :: Array JSX)
type HoveringDropdownPropsImpl = MantineComponentImpl (children :: Array JSX)
