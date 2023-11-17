module Mantine.Core.Combobox.Combobox
  ( combobox
  , ComboboxProps
  , ComboboxArrowPosition(..)
  , ComboboxFloatingPosition(..)
  , ComboboxPopoverWidth(..)
  , FloatingAxesOffsets
  , Offset(..)

  , ComboboxStore
  , ComboboxDropdownEventSource(..)
  , ComboboxSelectedOption(..)

  , comboboxOption
  , ComboboxOptionProps

  , comboboxTarget
  , ComboboxTargetProps

  , comboboxDropdownTarget
  , ComboboxDropdownTargetProps

  , comboboxEventsTarget
  , ComboboxEventsTargetProps

  , comboboxDropdown
  , ComboboxDropdownProps

  , comboboxGroup
  , ComboboxGroupProps

  , EventsTargetType(..)

  , ComboboxPropsImpl
  , ComboboxArrowPositionImpl
  , ComboboxFloatingPositionImpl
  , ComboboxPopoverWidthImpl
  , FloatingAxesOffsetsImpl
  , OffsetImpl
  , ComboboxStoreImpl
  , ComboboxDropdownEventSourceImpl
  , ComboboxSelectedOptionImpl
  ) where

import Mantine.Core.Prelude
import Web.HTML.HTMLElement (HTMLElement)
import Web.HTML.HTMLInputElement (HTMLInputElement)

combobox :: (ComboboxProps -> ComboboxProps) -> JSX
combobox = mkComponentWithDefault comboboxComponent defaultComboboxProps

foreign import comboboxComponent :: ReactComponent ComboboxPropsImpl

defaultComboboxProps :: ComboboxProps
defaultComboboxProps =
  defaultMantineComponent
    { onClose: pure unit
    , onOpen:  pure unit
    , onOptionSubmit: const (const (pure unit))
    }

-- Not supported properties
--   { portalProps          :: Omit<PortalProps, "children">
--   , positionDependencies :: any[]
--   }

type ComboboxProps =
  MantineComponent
    ( arrowOffset                 :: Optional Pixels
    , arrowPosition               :: Optional ComboboxArrowPosition
    , arrowRadius                 :: Optional Pixels
    , arrowSize                   :: Optional Pixels
    , children                    :: Array JSX
    , disabled                    :: Boolean
    , dropdownPadding             :: Optional Number
    , keepMounted                 :: Boolean
    , middlewares                 :: PopoverMiddlewares
    , offset                      :: Optional Offset
    , onClose                     :: Effect Unit
    , onOpen                      :: Effect Unit
    , onOptionSubmit              :: String -> ComboboxOptionProps -> Effect Unit
    , onPositionChange            :: ValueHandler ComboboxFloatingPosition
    , position                    :: ComboboxFloatingPosition
    , radius                      :: Optional MantineNumberSize
    , readOnly                    :: Boolean
    , resetSelectionOnOptionHover :: Boolean
    , returnFocus                 :: Boolean
    , shadow                      :: Optional MantineShadow
    , size                        :: Optional MantineSize
    , store                       :: Optional ComboboxStore
    , transitionProps             :: MantineTransitionProps
    , width                       :: Optional ComboboxPopoverWidth
    , withArrow                   :: Boolean
    , withinPortal                :: Boolean
    , zIndex                      :: Optional ZIndex
    )

type ComboboxStore =
  { clickSelectedOption       :: Effect Unit
  , closeDropdown             :: ValueHandler ComboboxDropdownEventSource
  , dropdownOpened            :: Boolean
  , focusSearchInput          :: Effect Unit
  , focusTarget               :: Effect Unit
  , listId                    :: Maybe String
  , openDropdown              :: ValueHandler ComboboxDropdownEventSource
  , resetSelectedOption       :: Effect Unit
  , searchRef                 :: Ref (Nullable HTMLInputElement)
  , selectActiveOption        :: Effect (Maybe String)
  , selectFirstOption         :: Effect (Maybe String)
  , selectNextOption          :: Effect (Maybe String)
  , selectOption              :: ValueHandler Number
  , selectPreviousOption      :: Effect (Maybe String)
  , selectedOptionIndex       :: Number
  , setListId                 :: ValueHandler String
  , targetRef                 :: Ref (Nullable HTMLElement)
  , toggleDropdown            :: ValueHandler ComboboxDropdownEventSource
  , updateSelectedOptionIndex :: ValueHandler ComboboxSelectedOption
  }

data ComboboxDropdownEventSource
  = ComboboxDropdownEventSourceKeyboard
  | ComboboxDropdownEventSourceMouse
  | ComboboxDropdownEventSourceUnknown

instance FromFFI ComboboxDropdownEventSourceImpl ComboboxDropdownEventSource where
  fromNative = case _ of
    "keyboard" -> ComboboxDropdownEventSourceKeyboard
    "mouse"    -> ComboboxDropdownEventSourceMouse
    "unknown"  -> ComboboxDropdownEventSourceUnknown
    _          -> ComboboxDropdownEventSourceUnknown

type ComboboxDropdownEventSourceImpl = String

instance ToFFI ComboboxDropdownEventSource ComboboxDropdownEventSourceImpl where
  toNative = case _ of
    ComboboxDropdownEventSourceKeyboard -> "keyboard"
    ComboboxDropdownEventSourceMouse    -> "mouse"
    ComboboxDropdownEventSourceUnknown  -> "unknown"

data ComboboxSelectedOption
  = ComboboxSelectedOptionActive
  | ComboboxSelectedOptionSelected

type ComboboxSelectedOptionImpl = String

instance ToFFI ComboboxSelectedOption ComboboxSelectedOptionImpl where
  toNative = case _ of
    ComboboxSelectedOptionActive   -> "active"
    ComboboxSelectedOptionSelected -> "selected"

instance FromFFI ComboboxSelectedOptionImpl ComboboxSelectedOption where
  fromNative = case _ of
    "active" -> ComboboxSelectedOptionActive
    _        -> ComboboxSelectedOptionSelected

type ComboboxStoreImpl =
  { clickSelectedOption       :: Effect Unit
  , closeDropdown             :: ValueHandlerImpl ComboboxDropdownEventSourceImpl
  , dropdownOpened            :: Boolean
  , focusSearchInput          :: Effect Unit
  , focusTarget               :: Effect Unit
  , listId                    :: Nullable String
  , openDropdown              :: ValueHandlerImpl ComboboxDropdownEventSourceImpl
  , resetSelectedOption       :: Effect Unit
  , searchRef                 :: Ref (Nullable HTMLInputElement)
  , selectActiveOption        :: Effect (Nullable String)
  , selectFirstOption         :: Effect (Nullable String)
  , selectNextOption          :: Effect (Nullable String)
  , selectOption              :: ValueHandlerImpl Number
  , selectPreviousOption      :: Effect (Nullable String)
  , selectedOptionIndex       :: Number
  , setListId                 :: ValueHandlerImpl String
  , targetRef                 :: Ref (Nullable HTMLElement)
  , toggleDropdown            :: ValueHandlerImpl ComboboxDropdownEventSourceImpl
  , updateSelectedOptionIndex :: ValueHandlerImpl ComboboxSelectedOptionImpl
  }

data Offset
  = InPixels Pixels
  | ByAxes FloatingAxesOffsets

type OffsetImpl = Number |+| FloatingAxesOffsetsImpl

instance ToFFI Offset OffsetImpl where
  toNative = case _ of
    InPixels p -> asOneOf (toNative p)
    ByAxes fao -> asOneOf (toNative fao)

type FloatingAxesOffsets =
  { mainAxis      :: Optional Number
  , crossAxis     :: Optional Number
  , alignmentAxis :: Optional Number
  }

type FloatingAxesOffsetsImpl =
  { mainAxis      :: OptionalImpl Number
  , crossAxis     :: OptionalImpl Number
  , alignmentAxis :: OptionalImpl Number
  }

data ComboboxPopoverWidth
  = ComboboxPopoverWidthTarget
  | ComboboxPopoverWidthNative String

type ComboboxPopoverWidthImpl = String

instance ToFFI ComboboxPopoverWidth ComboboxPopoverWidthImpl where
  toNative = case _ of
    ComboboxPopoverWidthTarget   -> "target"
    ComboboxPopoverWidthNative n -> n

data ComboboxFloatingPosition
  = ComboboxFloatingPositionTop
  | ComboboxFloatingPositionRight
  | ComboboxFloatingPositionBottom
  | ComboboxFloatingPositionLeft
  | ComboboxFloatingPositionTopStart
  | ComboboxFloatingPositionRightStart
  | ComboboxFloatingPositionBottomStart
  | ComboboxFloatingPositionLeftStart
  | ComboboxFloatingPositionTopEnd
  | ComboboxFloatingPositionRightEnd
  | ComboboxFloatingPositionBottomEnd
  | ComboboxFloatingPositionLeftEnd

instance DefaultValue ComboboxFloatingPosition where
  defaultValue = ComboboxFloatingPositionBottom

type ComboboxFloatingPositionImpl = String

instance ToFFI ComboboxFloatingPosition ComboboxFloatingPositionImpl where
  toNative = case _ of
    ComboboxFloatingPositionTop         -> "top"
    ComboboxFloatingPositionRight       -> "right"
    ComboboxFloatingPositionBottom      -> "bottom"
    ComboboxFloatingPositionLeft        -> "left"
    ComboboxFloatingPositionTopStart    -> "top-start"
    ComboboxFloatingPositionRightStart  -> "right-start"
    ComboboxFloatingPositionBottomStart -> "bottom-start"
    ComboboxFloatingPositionLeftStart   -> "left-start"
    ComboboxFloatingPositionTopEnd      -> "top-end"
    ComboboxFloatingPositionRightEnd    -> "right-end"
    ComboboxFloatingPositionBottomEnd   -> "bottom-end"
    ComboboxFloatingPositionLeftEnd     -> "left-end"

instance FromFFI String ComboboxFloatingPosition where
  fromNative = case _ of
    "top"          -> ComboboxFloatingPositionTop
    "right"        -> ComboboxFloatingPositionRight
    "bottom"       -> ComboboxFloatingPositionBottom
    "left"         -> ComboboxFloatingPositionLeft
    "top-start"    -> ComboboxFloatingPositionTopStart
    "right-start"  -> ComboboxFloatingPositionRightStart
    "bottom-start" -> ComboboxFloatingPositionBottomStart
    "left-start"   -> ComboboxFloatingPositionLeftStart
    "top-end"      -> ComboboxFloatingPositionTopEnd
    "right-end"    -> ComboboxFloatingPositionRightEnd
    "bottom-end"   -> ComboboxFloatingPositionBottomEnd
    "left-end"     -> ComboboxFloatingPositionLeftEnd
    _              -> defaultValue

data ComboboxArrowPosition
  = ComboboxArrowPositionCenter
  | ComboboxArrowPositionSide

type ComboboxArrowPositionImpl = String

instance ToFFI ComboboxArrowPosition ComboboxArrowPositionImpl where
  toNative = case _ of
    ComboboxArrowPositionCenter -> "center"
    ComboboxArrowPositionSide   -> "side"

type ComboboxPropsImpl =
  MantineComponentImpl
    ( arrowOffset                 :: OptionalImpl PixelsImpl
    , arrowPosition               :: OptionalImpl ComboboxArrowPositionImpl
    , arrowRadius                 :: OptionalImpl PixelsImpl
    , arrowSize                   :: OptionalImpl PixelsImpl
    , children                    :: Array JSX
    , disabled                    :: Boolean
    , dropdownPadding             :: OptionalImpl Number
    , keepMounted                 :: Boolean
    , middlewares                 :: PopoverMiddlewaresImpl
    , offset                      :: OptionalImpl OffsetImpl
    , onClose                     :: Effect Unit
    , onOpen                      :: Effect Unit
    , onOptionSubmit              :: EffectFn2 String ComboboxOptionProps Unit
    , onPositionChange            :: ValueHandlerImpl ComboboxFloatingPositionImpl
    , position                    :: ComboboxFloatingPositionImpl
    , radius                      :: OptionalImpl MantineNumberSizeImpl
    , readOnly                    :: Boolean
    , resetSelectionOnOptionHover :: Boolean
    , returnFocus                 :: Boolean
    , shadow                      :: OptionalImpl MantineShadowImpl
    , size                        :: OptionalImpl MantineSizeImpl
    , store                       :: OptionalImpl ComboboxStoreImpl
    , transitionProps             :: MantineTransitionPropsImpl
    , width                       :: OptionalImpl ComboboxPopoverWidthImpl
    , withArrow                   :: Boolean
    , withinPortal                :: Boolean
    , zIndex                      :: OptionalImpl ZIndexImpl
    )

comboboxOption :: (ComboboxOptionProps -> ComboboxOptionProps) -> JSX
comboboxOption = mkTrivialComponent comboboxOptionComponent

foreign import comboboxOptionComponent :: ReactComponent ComboboxOptionPropsImpl

type ComboboxOptionProps =
  MantineComponent
    ( active   :: Boolean
    , disabled :: Boolean
    , selected :: Boolean
    , value    :: Optional String
    )

type ComboboxOptionPropsImpl =
  MantineComponentImpl
    ( active   :: Boolean
    , disabled :: Boolean
    , selected :: Boolean
    , value    :: OptionalImpl String
    )

comboboxTarget :: (ComboboxTargetProps -> ComboboxTargetProps) -> JSX
comboboxTarget = mkTrivialComponent comboboxTargetComponent

foreign import comboboxTargetComponent :: ReactComponent ComboboxTargetPropsImpl

type ComboboxTargetProps =
  MantineComponent
    ( children               :: Array JSX
    , refProp                :: Optional String
    , targetType             :: EventsTargetType
    , withAriaAttributes     :: Boolean
    , withExpandedAttribute  :: Boolean
    , withKeyboardNavigation :: Boolean
    )

type ComboboxTargetPropsImpl =
  MantineComponentImpl
    ( children               :: Array JSX
    , refProp                :: OptionalImpl String
    , targetType             :: EventsTargetTypeImpl
    , withAriaAttributes     :: Boolean
    , withExpandedAttribute  :: Boolean
    , withKeyboardNavigation :: Boolean
    )

comboboxDropdownTarget :: (ComboboxDropdownTargetProps -> ComboboxDropdownTargetProps) -> JSX
comboboxDropdownTarget = mkTrivialComponent comboboxDropdownTargetComponent

foreign import comboboxDropdownTargetComponent :: ReactComponent ComboboxDropdownTargetPropsImpl

type ComboboxDropdownTargetProps =
  MantineComponent
    ( children :: Array JSX
    , refProp  :: Optional String
    )

type ComboboxDropdownTargetPropsImpl =
  MantineComponentImpl
    ( children :: Array JSX
    , refProp  :: OptionalImpl String
    )

comboboxEventsTarget :: (ComboboxEventsTargetProps -> ComboboxEventsTargetProps) -> JSX
comboboxEventsTarget = mkTrivialComponent comboboxEventsTargetComponent

foreign import comboboxEventsTargetComponent :: ReactComponent ComboboxEventsTargetPropsImpl

type ComboboxEventsTargetProps =
  MantineComponent
    ( children               :: Array JSX
    , refProp                :: Optional String
    , targetType             :: EventsTargetType
    , withAriaAttributes     :: Boolean
    , withExpandedAttribute  :: Boolean
    , withKeyboardNavigation :: Boolean
    )

type ComboboxEventsTargetPropsImpl =
  MantineComponentImpl
    ( children               :: Array JSX
    , refProp                :: OptionalImpl String
    , targetType             :: EventsTargetTypeImpl
    , withAriaAttributes     :: Boolean
    , withExpandedAttribute  :: Boolean
    , withKeyboardNavigation :: Boolean
    )

comboboxDropdown :: (ComboboxDropdownProps -> ComboboxDropdownProps) -> JSX
comboboxDropdown = mkTrivialComponent comboboxDropdownComponent

foreign import comboboxDropdownComponent :: ReactComponent ComboboxDropdownPropsImpl

type ComboboxDropdownProps     = MantineComponent     ( hidden :: Boolean )
type ComboboxDropdownPropsImpl = MantineComponentImpl ( hidden :: Boolean )

comboboxGroup :: (ComboboxGroupProps -> ComboboxGroupProps) -> JSX
comboboxGroup = mkTrivialComponent comboboxGroupComponent

foreign import comboboxGroupComponent :: ReactComponent ComboboxGroupPropsImpl

type ComboboxGroupProps     = MantineComponent     ( label :: Optional     JSX )
type ComboboxGroupPropsImpl = MantineComponentImpl ( label :: OptionalImpl JSX )

data EventsTargetType
  = EventsTargetTypeInput
  | EventsTargetTypeButton

instance DefaultValue EventsTargetType where
  defaultValue = EventsTargetTypeInput

type EventsTargetTypeImpl = String

instance ToFFI EventsTargetType EventsTargetTypeImpl where
  toNative = case _ of
    EventsTargetTypeInput  -> "input"
    EventsTargetTypeButton -> "button"
