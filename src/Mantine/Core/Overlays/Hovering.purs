module Mantine.Core.Overlays.Hovering
  ( hoverCard
  , Props_HoverCard
  , Props_HoverCardImpl

  , hoverCardTarget
  , Props_HoverTarget
  , Props_HoverTargetImpl

  , popover
  , Props_Popover

  , popoverTarget
  , Props_PopoverTarget
  , Props_PopoverTargetImpl

  , hoverCardDropdown
  , popoverDropdown
  , Props_HoveringDropdown
  , Props_HoveringDropdownImpl

  , HoveringCommons
  , HoveringCommonsImpl
  , HoveringTarget
  , HoveringTargetImpl
  , HoverableArrowPosition(..)
  , HoverableArrowPositionImpl
  , HoverableComponent
  , HoverableComponentImpl
  , HoverableFloatingPosition(..)
  , HoverableFloatingPositionImpl
  , HoverPopoverWidth(..)
  , HoverPopoverWidthImpl
  , HoverPopupType(..)
  , HoverPopupTypeImpl

  , Props_PopoverImpl
  ) where

import Mantine.Core.Overlays.Modal (ModalTransitionProps, ModalTransitionPropsImpl)
import Mantine.Core.Prelude

hoverCard
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_HoverCard
  => Union attrsImpl attrsImpl_ Props_HoverCardImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
hoverCard = element (unsafeCoerce hoverCardComponent) <<< toNative

foreign import hoverCardComponent :: ReactComponent (Record Props_HoverCardImpl)

type Props_HoverCard =
  HoveringCommons
    ( closeDelay      :: Milliseconds
    , initiallyOpened :: Boolean
    , openDelay       :: Milliseconds
    )

type Props_HoverCardImpl =
  HoveringCommonsImpl
    ( closeDelay      :: MillisecondsImpl
    , initiallyOpened :: Boolean
    , openDelay       :: MillisecondsImpl
    )

hoverCardTarget
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_HoverTarget
  => Union attrsImpl attrsImpl_ Props_HoverTargetImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
hoverCardTarget = element (unsafeCoerce hoverCardTargetComponent) <<< toNative

foreign import hoverCardTargetComponent :: ReactComponent (Record Props_HoverTargetImpl)

type Props_HoverTarget     = HoveringTarget     (eventPropsWrapperName :: String)
type Props_HoverTargetImpl = HoveringTargetImpl (eventPropsWrapperName :: String)

hoverCardDropdown
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_HoveringDropdown
  => Union attrsImpl attrsImpl_ Props_HoveringDropdownImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
hoverCardDropdown = element (unsafeCoerce hoverCardDropdownComponent) <<< toNative

foreign import hoverCardDropdownComponent :: ReactComponent (Record Props_HoveringDropdownImpl)

popover
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Popover
  => Union attrsImpl attrsImpl_ Props_PopoverImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
popover = element (unsafeCoerce popoverComponent) <<< toNative

foreign import popoverComponent :: ReactComponent (Record Props_PopoverImpl)

type Props_NonDefaultableHovering =
  { closeOnClickOutside :: Boolean
  , closeOnEscape       :: Boolean
  , onClose             :: Effect Unit
  , onOpen              :: Effect Unit
  , withRoles           :: Boolean
  , withinPortal        :: Boolean
  }

type Props_Popover     = HoveringCommons     ()
type Props_PopoverImpl = HoveringCommonsImpl ()

popoverTarget
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_PopoverTarget
  => Union attrsImpl attrsImpl_ Props_PopoverTargetImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
popoverTarget = element (unsafeCoerce popoverTargetComponent) <<< toNative

type Props_PopoverTarget     = HoveringTarget     ()
type Props_PopoverTargetImpl = HoveringTargetImpl ()

foreign import popoverTargetComponent :: ReactComponent (Record Props_PopoverTargetImpl)

popoverDropdown
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_HoveringDropdown
  => Union attrsImpl attrsImpl_ Props_HoveringDropdownImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
popoverDropdown = element (unsafeCoerce popoverDropdownComponent) <<< toNative

foreign import popoverDropdownComponent :: ReactComponent (Record Props_HoveringDropdownImpl)

type HoverableComponent rest =
  Props_Common
    ( arrowOffset          :: Number
    , arrowPosition        :: HoverableArrowPosition
    , arrowRadius          :: Number
    , arrowSize            :: Number
    , children             :: Array JSX
    , disabled             :: Boolean
    , keepMounted          :: Boolean
    , offset               :: Number
    , onPositionChange     :: ValueHandler HoverableFloatingPosition
    , opened               :: Boolean
    , position             :: HoverableFloatingPosition
 -- , positionDependencies :: any[] -- TODO
    , radius               :: MantineNumberSize
    , transitionProps      :: ModalTransitionProps
    , withArrow            :: Boolean
    , withinPortal         :: Boolean
    , zIndex               :: ZIndex
    | rest
    )

type HoveringCommons rest =
  HoverableComponent
    ( clickOutsideEvents  :: Array String
    , closeOnClickOutside :: Boolean
    , closeOnEscape       :: Boolean
    , defaultOpened       :: Boolean
    , id                  :: String
 -- , middlewares         :: PopoverMiddlewares -- TODO
    , onChange            :: ValueHandler Boolean
    , onClose             :: Effect Unit
    , onOpen              :: Effect Unit
    , returnFocus         :: Boolean
    , shadow              :: MantineShadow
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
    _              -> HoverableFloatingPositionBottom

data HoverPopoverWidth = AsTarget | FixedWidth Number

type HoverPopoverWidthImpl = String |+| Number

instance ToFFI HoverPopoverWidth HoverPopoverWidthImpl where
  toNative = case _ of
    AsTarget     -> asOneOf "target"
    FixedWidth n -> asOneOf n

type HoverableComponentImpl rest =
  Props_CommonImpl
    ( arrowOffset          :: Number
    , arrowPosition        :: HoverableArrowPositionImpl
    , arrowRadius          :: Number
    , arrowSize            :: Number
    , children             :: Array JSX
    , disabled             :: Boolean
    , keepMounted          :: Boolean
    , offset               :: Number
    , onPositionChange     :: ValueHandlerImpl HoverableFloatingPositionImpl
    , opened               :: Boolean
    , position             :: HoverableFloatingPositionImpl
 -- , positionDependencies :: any[] -- TODO
    , radius               :: MantineNumberSizeImpl
    , transitionProps      :: ModalTransitionPropsImpl
    , withArrow            :: Boolean
    , withinPortal         :: Boolean
    , zIndex               :: ZIndexImpl
    | rest
    )

type HoveringCommonsImpl rest =
  HoverableComponentImpl
    ( clickOutsideEvents  :: Array String
    , closeOnClickOutside :: Boolean
    , closeOnEscape       :: Boolean
    , defaultOpened       :: Boolean
    , id                  :: String
 -- , middlewares         :: PopoverMiddlewares -- TODO
    , onChange            :: ValueHandlerImpl Boolean
    , onClose             :: Effect Unit
    , onOpen              :: Effect Unit
    , returnFocus         :: Boolean
    , shadow              :: MantineShadowImpl
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
  Props_Common
    ( children  :: Array JSX
    , popupType :: HoverPopupType
    , refProp   :: String
    | rest
    )

data HoverPopupType
  = HoverPopupTypeDialog
  | HoverPopupTypeCustom String

type HoverPopupTypeImpl = String

instance ToFFI HoverPopupType HoverPopupTypeImpl where
  toNative = case _ of
    HoverPopupTypeDialog   -> "dialog"
    HoverPopupTypeCustom s -> s

type HoveringTargetImpl rest =
  Props_CommonImpl
    ( children  :: Array JSX
    , popupType :: HoverPopupTypeImpl
    , refProp   :: String
    | rest
    )

type Props_HoveringDropdown     = Props_Common     (children :: Array JSX)
type Props_HoveringDropdownImpl = Props_CommonImpl (children :: Array JSX)
