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
  , FloatingAxesOffsetsImpl
  , OffsetImpl
  , ComboboxStoreImpl
  ) where

import Effect.Uncurried (EffectFn2)
import Mantine.Core.Prelude
import Web.HTML.HTMLElement (HTMLElement)
import Web.HTML.HTMLInputElement (HTMLInputElement)

combobox :: (ComboboxProps -> ComboboxProps) -> JSX
combobox = mkComponentWithDefault comboboxComponent defaultComboboxProps

foreign import comboboxComponent :: ReactComponent ComboboxPropsImpl

defaultComboboxProps :: ComboboxProps
defaultComboboxProps =
  defaultThemingProps
    { onClose: pure unit
    , onOpen:  pure unit
    , onOptionSubmit: const (const (pure unit))
    }

-- Not supported properties
--   { portalProps          :: Omit<PortalProps, "children">
--   , positionDependencies :: any[]
--   }

type ComboboxProps =
  ThemingProps
    ( arrowOffset                 :: Maybe Pixels
    , arrowPosition               :: Maybe ComboboxArrowPosition
    , arrowRadius                 :: Maybe Pixels
    , arrowSize                   :: Maybe Pixels
    , children                    :: Array JSX
    , disabled                    :: Boolean
    , dropdownPadding             :: Maybe Number
    , keepMounted                 :: Boolean
    , middlewares                 :: PopoverMiddlewares
    , offset                      :: Maybe Offset
    , onClose                     :: Effect Unit
    , onOpen                      :: Effect Unit
    , onOptionSubmit              :: String -> ComboboxOptionProps -> Effect Unit
    , onPositionChange            :: ValueHandler ComboboxFloatingPosition
    , position                    :: ComboboxFloatingPosition
    , radius                      :: Maybe MantineNumberSize
    , readOnly                    :: Boolean
    , resetSelectionOnOptionHover :: Boolean
    , returnFocus                 :: Boolean
    , size                        :: Maybe MantineSize
    , shadow                      :: Maybe MantineShadow
    , store                       :: Maybe ComboboxStore
    , transitionProps             :: MantineTransitionProps
    , width                       :: Maybe ComboboxPopoverWidth
    , withArrow                   :: Boolean
    , withinPortal                :: Boolean
    , zIndex                      :: Maybe Number
    )

type ComboboxStore =
  { dropdownOpened            :: Boolean
  , openDropdown              :: ValueHandler ComboboxDropdownEventSource
  , closeDropdown             :: ValueHandler ComboboxDropdownEventSource
  , toggleDropdown            :: ValueHandler ComboboxDropdownEventSource
  , selectedOptionIndex       :: Maybe Number
  , selectOption              :: ValueHandler Number
  , selectActiveOption        :: Effect (Maybe String)
  , selectFirstOption         :: Effect (Maybe String)
  , selectNextOption          :: Effect (Maybe String)
  , selectPreviousOption      :: Effect (Maybe String)
  , resetSelectedOption       :: Effect Unit
  , clickSelectedOption       :: Effect Unit
  , updateSelectedOptionIndex :: ValueHandler ComboboxSelectedOption
  , listId                    :: Maybe String
  , setListId                 :: ValueHandler String
  , searchRef                 :: Maybe (Ref HTMLInputElement)
  , focusSearchInput          :: Effect Unit
  , targetRef                 :: Maybe (Ref HTMLElement)
  , focusTarget               :: Effect Unit
  }

data ComboboxDropdownEventSource
  = ComboboxDropdownEventSourceKeyboard
  | ComboboxDropdownEventSourceMouse
  | ComboboxDropdownEventSourceUnknown

instance FromFFI String ComboboxDropdownEventSource where
  fromNative = case _ of
    "keyboard" -> ComboboxDropdownEventSourceKeyboard
    "mouse"    -> ComboboxDropdownEventSourceMouse
    "unknown"  -> ComboboxDropdownEventSourceUnknown
    _          -> ComboboxDropdownEventSourceUnknown

instance ToFFI ComboboxDropdownEventSource String where
  toNative = case _ of
    ComboboxDropdownEventSourceKeyboard -> "keyboard"
    ComboboxDropdownEventSourceMouse    -> "mouse"
    ComboboxDropdownEventSourceUnknown  -> "unknown"

data ComboboxSelectedOption
  = ComboboxSelectedOptionActive
  | ComboboxSelectedOptionSelected

instance ToFFI ComboboxSelectedOption String where
  toNative = case _ of
    ComboboxSelectedOptionActive   -> "active"
    ComboboxSelectedOptionSelected -> "selected"

instance FromFFI String ComboboxSelectedOption where
  fromNative = case _ of
    "active" -> ComboboxSelectedOptionActive
    _        -> ComboboxSelectedOptionSelected

type ComboboxStoreImpl =
  { dropdownOpened            :: Boolean
  , openDropdown              :: EffectFn1 String Unit
  , closeDropdown             :: EffectFn1 String Unit
  , toggleDropdown            :: EffectFn1 String Unit
  , selectedOptionIndex       :: Nullable Number
  , selectOption              :: EffectFn1 Number Unit
  , selectActiveOption        :: Effect (Nullable String)
  , selectFirstOption         :: Effect (Nullable String)
  , selectNextOption          :: Effect (Nullable String)
  , selectPreviousOption      :: Effect (Nullable String)
  , resetSelectedOption       :: Effect Unit
  , clickSelectedOption       :: Effect Unit
  , updateSelectedOptionIndex :: EffectFn1 String Unit
  , listId                    :: Nullable String
  , setListId                 :: EffectFn1 String Unit
  , searchRef                 :: Nullable (Ref HTMLInputElement)
  , focusSearchInput          :: Effect Unit
  , targetRef                 :: Nullable (Ref HTMLElement)
  , focusTarget               :: Effect Unit
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
  { mainAxis      :: Maybe Number
  , crossAxis     :: Maybe Number
  , alignmentAxis :: Maybe Number
  }

type FloatingAxesOffsetsImpl =
  { mainAxis      :: Nullable Number
  , crossAxis     :: Nullable Number
  , alignmentAxis :: Nullable Number
  }

data ComboboxPopoverWidth
  = ComboboxPopoverWidthTarget
  | ComboboxPopoverWidthNative String

instance ToFFI ComboboxPopoverWidth String where
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

instance DefaultValue ComboboxFloatingPosition where defaultValue = ComboboxFloatingPositionBottom

instance ToFFI ComboboxFloatingPosition String where
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

instance ToFFI ComboboxArrowPosition String where
  toNative = case _ of
    ComboboxArrowPositionCenter -> "center"
    ComboboxArrowPositionSide   -> "side"

type ComboboxPropsImpl =
  ThemingPropsImpl
    ( arrowOffset                 :: Nullable Number
    , arrowPosition               :: Nullable String
    , arrowRadius                 :: Nullable Number
    , arrowSize                   :: Nullable Number
    , children                    :: Array JSX
    , disabled                    :: Boolean
    , dropdownPadding             :: Nullable Number
    , keepMounted                 :: Boolean
    , middlewares                 :: PopoverMiddlewaresImpl
    , offset                      :: Nullable OffsetImpl
    , onClose                     :: Effect Unit
    , onOpen                      :: Effect Unit
    , onOptionSubmit              :: EffectFn2 String ComboboxOptionProps Unit
    , onPositionChange            :: EffectFn1 String Unit
    , position                    :: String
    , radius                      :: Nullable MantineNumberSizeImpl
    , readOnly                    :: Boolean
    , resetSelectionOnOptionHover :: Boolean
    , returnFocus                 :: Boolean
    , size                        :: Nullable String
    , shadow                      :: Nullable String
    , store                       :: Nullable ComboboxStoreImpl
    , transitionProps             :: MantineTransitionPropsImpl
    , width                       :: Nullable String
    , withArrow                   :: Boolean
    , withinPortal                :: Boolean
    , zIndex                      :: Nullable Number
    )

comboboxOption :: (ComboboxOptionProps -> ComboboxOptionProps) -> JSX
comboboxOption = mkTrivialComponent comboboxOptionComponent

foreign import comboboxOptionComponent :: ReactComponent ComboboxOptionPropsImpl

type ComboboxOptionProps =
  ThemingProps
    ( active   :: Boolean
    , disabled :: Boolean
    , selected :: Boolean
    , value    :: Maybe String
    )

type ComboboxOptionPropsImpl =
  ThemingPropsImpl
    ( active   :: Boolean
    , disabled :: Boolean
    , selected :: Boolean
    , value    :: Nullable String
    )

comboboxTarget :: (ComboboxTargetProps -> ComboboxTargetProps) -> JSX
comboboxTarget = mkTrivialComponent comboboxTargetComponent

foreign import comboboxTargetComponent :: ReactComponent ComboboxTargetPropsImpl

type ComboboxTargetProps =
  ThemingProps
    ( children               :: Array JSX
    , refProp                :: Maybe String
    , targetType             :: EventsTargetType
    , withAriaAttributes     :: Boolean
    , withExpandedAttribute  :: Boolean
    , withKeyboardNavigation :: Boolean
    )

type ComboboxTargetPropsImpl =
  ThemingPropsImpl
    ( children               :: Array JSX
    , refProp                :: Nullable String
    , targetType             :: String
    , withAriaAttributes     :: Boolean
    , withExpandedAttribute  :: Boolean
    , withKeyboardNavigation :: Boolean
    )

comboboxDropdownTarget :: (ComboboxDropdownTargetProps -> ComboboxDropdownTargetProps) -> JSX
comboboxDropdownTarget = mkTrivialComponent comboboxDropdownTargetComponent

foreign import comboboxDropdownTargetComponent :: ReactComponent ComboboxDropdownTargetPropsImpl

type ComboboxDropdownTargetProps =
  ThemingProps
    ( children :: Array JSX
    , refProp  :: Maybe String
    )

type ComboboxDropdownTargetPropsImpl =
  ThemingPropsImpl
    ( children :: Array JSX
    , refProp  :: Nullable String
    )

comboboxEventsTarget :: (ComboboxEventsTargetProps -> ComboboxEventsTargetProps) -> JSX
comboboxEventsTarget = mkTrivialComponent comboboxEventsTargetComponent

foreign import comboboxEventsTargetComponent :: ReactComponent ComboboxEventsTargetPropsImpl

type ComboboxEventsTargetProps =
  ThemingProps
    ( children               :: Array JSX
    , refProp                :: Maybe String
    , targetType             :: EventsTargetType
    , withAriaAttributes     :: Boolean
    , withExpandedAttribute  :: Boolean
    , withKeyboardNavigation :: Boolean
    )

type ComboboxEventsTargetPropsImpl =
  ThemingPropsImpl
    ( children               :: Array JSX
    , refProp                :: Nullable String
    , targetType             :: String
    , withAriaAttributes     :: Boolean
    , withExpandedAttribute  :: Boolean
    , withKeyboardNavigation :: Boolean
    )

comboboxDropdown :: (ComboboxDropdownProps -> ComboboxDropdownProps) -> JSX
comboboxDropdown = mkTrivialComponent comboboxDropdownComponent

foreign import comboboxDropdownComponent :: ReactComponent ComboboxDropdownPropsImpl

type ComboboxDropdownProps =
  ThemingProps
    ( hidden :: Boolean
    )

type ComboboxDropdownPropsImpl =
  ThemingPropsImpl
    ( hidden :: Boolean
    )

comboboxGroup :: (ComboboxGroupProps -> ComboboxGroupProps) -> JSX
comboboxGroup = mkTrivialComponent comboboxGroupComponent

foreign import comboboxGroupComponent :: ReactComponent ComboboxGroupPropsImpl

type ComboboxGroupProps =
  ThemingProps
    ( label :: Maybe JSX
    )

type ComboboxGroupPropsImpl =
  ThemingPropsImpl
    ( label :: Nullable JSX
    )

data EventsTargetType = EventsTargetTypeInput | EventsTargetTypeButton

instance ToFFI EventsTargetType String where
  toNative = case _ of
    EventsTargetTypeInput  -> "input"
    EventsTargetTypeButton -> "button"

instance DefaultValue EventsTargetType where defaultValue = EventsTargetTypeInput
