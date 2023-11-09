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
  , HoveringTarget
  , HoverableArrowPosition(..)
  , HoverableArrowPositionImpl
  , HoverableComponent
  , HoverableComponentImpl
  , HoverableFloatingPosition(..)
  , HoverableFloatingPositionImpl
  , HoverPopoverWidth(..)
  , HoverPopupType(..)
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
    ( closeDelay      :: Maybe Milliseconds
    , initiallyOpened :: Maybe Boolean
    , openDelay       :: Maybe Milliseconds
    )

type HoverCardPropsImpl =
  HoveringCommonsImpl
    ( closeDelay      :: Nullable MillisecondsImpl
    , initiallyOpened :: Nullable Boolean
    , openDelay       :: Nullable MillisecondsImpl
    )

hoverCardTarget :: (HoverTargetProps -> HoverTargetProps) -> JSX
hoverCardTarget = mkComponent hoverCardTargetComponent hoveringTargetToImpl defaultMantineComponent_

foreign import hoverCardTargetComponent :: ReactComponent HoverTargetPropsImpl

type HoverTargetProps     = HoveringTarget     (eventPropsWrapperName :: Maybe    String)
type HoverTargetPropsImpl = HoveringTargetImpl (eventPropsWrapperName :: Nullable String)

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
    , zIndex               :: Maybe ZIndex
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
    ( arrowOffset          :: Nullable Number
    , arrowPosition        :: Nullable HoverableArrowPositionImpl
    , arrowRadius          :: Nullable Number
    , arrowSize            :: Nullable Number
    , children             :: Array JSX
    , disabled             :: Boolean
    , keepMounted          :: Boolean
    , offset               :: Nullable Number
    , onPositionChange     :: ValueHandlerImpl HoverableFloatingPositionImpl
    , opened               :: Nullable Boolean
    , position             :: HoverableFloatingPositionImpl
 -- , positionDependencies :: any[] -- TODO
    , radius               :: Nullable MantineNumberSizeImpl
    , transitionProps      :: ModalTransitionPropsImpl
    , withArrow            :: Boolean
    , withinPortal         :: Boolean
    , zIndex               :: Nullable ZIndexImpl
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
    , onChange            :: ValueHandlerImpl Boolean
    , onClose             :: Effect Unit
    , onOpen              :: Effect Unit
    , returnFocus         :: Boolean
    , shadow              :: Nullable MantineShadowImpl
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
    , refProp   :: Maybe String
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
    , refProp   :: Nullable String
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
