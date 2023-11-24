module Mantine.Core.Combobox.Combobox
  ( combobox
  , Props_Combobox
  , Props_ComboboxImpl
  , ComboboxArrowPosition(..)
  , ComboboxArrowPositionImpl
  , ComboboxFloatingPosition(..)
  , ComboboxFloatingPositionImpl
  , ComboboxPopoverWidth(..)
  , ComboboxPopoverWidthImpl
  , FloatingAxesOffsets
  , FloatingAxesOffsetsImpl
  , Offset(..)
  , OffsetImpl
  , Props_ComboboxInner
  , Props_ComboboxInnerImpl

  , useCombobox
  , UseCombobox
  , Options_UseCombobox
  , Options_UseComboboxImpl
  , ComboboxStore
  , ComboboxStoreImpl
  , ComboboxDropdownEventSource(..)
  , ComboboxDropdownEventSourceImpl
  , ComboboxSelectedOption(..)
  , ComboboxSelectedOptionImpl
  , ScrollBehavior(..)
  , ScrollBehaviorImpl

  , comboboxOptions
  , comboboxOptions_
  , Props_ComboboxOptions
  , Props_ComboboxOptionsImpl

  , comboboxOption
  , Props_ComboboxOption
  , Props_ComboboxOptionRow
  , Props_ComboboxOptionImpl
  , Props_ComboboxOptionRowImpl

  , comboboxTarget
  , Props_ComboboxTarget
  , Props_ComboboxTargetImpl

  , comboboxDropdownTarget
  , comboboxDropdownTarget_
  , Props_ComboboxDropdownTarget
  , Props_ComboboxDropdownTargetImpl

  , comboboxEventsTarget
  , comboboxEventsTarget_
  , Props_ComboboxEventsTarget
  , Props_ComboboxEventsTargetImpl

  , comboboxDropdown
  , comboboxDropdown_
  , Props_ComboboxDropdown
  , Props_ComboboxDropdownImpl

  , comboboxGroup
  , Props_ComboboxGroup
  , Props_ComboboxGroupImpl

  , EventsTargetType(..)
  , EventsTargetTypeImpl
  ) where

import Mantine.Core.Prelude
import Effect.Uncurried ( runEffectFn1)
import React.Basic.Hooks (Hook, unsafeHook)
import Web.HTML.HTMLElement (HTMLElement)
import Web.HTML.HTMLInputElement (HTMLInputElement)

type Options_UseCombobox =
  ( defaultOpened   :: Boolean
  , opened          :: Boolean
  , onOpenedChange  :: ComboboxStore -> Boolean                     -> Effect Unit
  , onDropdownClose :: ComboboxStore -> ComboboxDropdownEventSource -> Effect Unit
  , onDropdownOpen  :: ComboboxStore -> ComboboxDropdownEventSource -> Effect Unit
  , loop            :: Boolean
  , scrollBehavior  :: ScrollBehavior
  )

type Options_UseComboboxImpl =
  ( defaultOpened   :: Boolean
  , opened          :: Boolean
  , onOpenedChange  :: EffectFn2 ComboboxStoreImpl Boolean                         Unit
  , onDropdownClose :: EffectFn2 ComboboxStoreImpl ComboboxDropdownEventSourceImpl Unit
  , onDropdownOpen  :: EffectFn2 ComboboxStoreImpl ComboboxDropdownEventSourceImpl Unit
  , loop            :: Boolean
  , scrollBehavior  :: ScrollBehaviorImpl
  )

data ScrollBehavior
  = ScrollBehaviorSmooth
  | ScrollBehaviorInstant
  | ScrollBehaviorAuto

type ScrollBehaviorImpl = String

instance ToFFI ScrollBehavior ScrollBehaviorImpl where
  toNative = case _ of
    ScrollBehaviorSmooth  -> "smooth"
    ScrollBehaviorInstant -> "instant"
    ScrollBehaviorAuto    -> "auto"

foreign import useComboboxImpl :: EffectFn1 (Record Options_UseComboboxImpl) ComboboxStoreImpl
foreign import data UseCombobox :: Type -> Type

useCombobox :: forall opts opts_ optsImpl optsImpl_
             . Union opts     opts_     Options_UseCombobox
            => Union optsImpl optsImpl_ Options_UseComboboxImpl
            => ToFFI (Record opts)  (Record optsImpl)
            => ToFFI (Record opts_) (Record optsImpl_)
            => Record opts -> Hook UseCombobox ComboboxStore
useCombobox arg1 = unsafeHook (fromNative <$> runEffectFn1 useComboboxImpl (unsafeCoerce (toNative arg1)))

combobox
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Combobox
  => Union attrsImpl attrsImpl_ Props_ComboboxImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
combobox = element (unsafeCoerce comboboxComponent) <<< toNative

foreign import comboboxComponent :: ReactComponent (Record Props_ComboboxImpl)

-- Not supported properties
--   { portalProps          :: Omit<PortalProps, "children">
--   , positionDependencies :: any[]
--   }

type Props_Combobox =
  Props_Common
    ( arrowOffset                 :: Pixels
    , arrowPosition               :: ComboboxArrowPosition
    , arrowRadius                 :: Pixels
    , arrowSize                   :: Pixels
    , children                    :: Array JSX
    , disabled                    :: Boolean
    , dropdownPadding             :: Number
    , keepMounted                 :: Boolean
    , middlewares                 :: PopoverMiddlewares
    , offset                      :: Offset
    , onClose                     :: Effect Unit
    , onOpen                      :: Effect Unit
    , onOptionSubmit              :: String -> Record Props_ComboboxOptionRow -> Effect Unit
    , onPositionChange            :: ValueHandler ComboboxFloatingPosition
    , position                    :: ComboboxFloatingPosition
    , radius                      :: MantineNumberSize
    , readOnly                    :: Boolean
    , resetSelectionOnOptionHover :: Boolean
    , returnFocus                 :: Boolean
    , shadow                      :: MantineShadow
    , size                        :: MantineSize
    , store                       :: ComboboxStore
    , transitionProps             :: MantineTransitionProps
    , width                       :: ComboboxPopoverWidth
    , withArrow                   :: Boolean
    , withinPortal                :: Boolean
    , zIndex                      :: ZIndex
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
  , updateSelectedOptionIndex :: ValueHandler (Maybe ComboboxSelectedOption)
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
  , updateSelectedOptionIndex :: ValueHandlerImpl (Nullable ComboboxSelectedOptionImpl)
  }

data Offset
  = OffsetInPixels Pixels
  | OffsetByAxes FloatingAxesOffsets

type OffsetImpl = Number |+| FloatingAxesOffsetsImpl

instance ToFFI Offset OffsetImpl where
  toNative = case _ of
    OffsetInPixels p -> asOneOf (toNative p)
    OffsetByAxes fao -> asOneOf (toNative fao)

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
    _              -> ComboboxFloatingPositionBottom

data ComboboxArrowPosition
  = ComboboxArrowPositionCenter
  | ComboboxArrowPositionSide

type ComboboxArrowPositionImpl = String

instance ToFFI ComboboxArrowPosition ComboboxArrowPositionImpl where
  toNative = case _ of
    ComboboxArrowPositionCenter -> "center"
    ComboboxArrowPositionSide   -> "side"

type Props_ComboboxImpl =
  Props_CommonImpl
    ( arrowOffset                 :: PixelsImpl
    , arrowPosition               :: ComboboxArrowPositionImpl
    , arrowRadius                 :: PixelsImpl
    , arrowSize                   :: PixelsImpl
    , children                    :: Array JSX
    , disabled                    :: Boolean
    , dropdownPadding             :: Number
    , keepMounted                 :: Boolean
    , middlewares                 :: PopoverMiddlewaresImpl
    , offset                      :: OffsetImpl
    , onClose                     :: Effect Unit
    , onOpen                      :: Effect Unit
    , onOptionSubmit              :: EffectFn2 String (Record Props_ComboboxOptionRowImpl) Unit
    , onPositionChange            :: ValueHandlerImpl ComboboxFloatingPositionImpl
    , position                    :: ComboboxFloatingPositionImpl
    , radius                      :: MantineNumberSizeImpl
    , readOnly                    :: Boolean
    , resetSelectionOnOptionHover :: Boolean
    , returnFocus                 :: Boolean
    , shadow                      :: MantineShadowImpl
    , size                        :: MantineSizeImpl
    , store                       :: ComboboxStoreImpl
    , transitionProps             :: MantineTransitionPropsImpl
    , width                       :: ComboboxPopoverWidthImpl
    , withArrow                   :: Boolean
    , withinPortal                :: Boolean
    , zIndex                      :: ZIndexImpl
    )

comboboxOptions
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_ComboboxOptions
  => Union attrsImpl attrsImpl_ Props_ComboboxOptionsImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
comboboxOptions = element (unsafeCoerce comboboxOptionsComponent) <<< toNative

comboboxOptions_ :: Array JSX -> JSX
comboboxOptions_ children = comboboxOptions { children }

foreign import comboboxOptionsComponent :: ReactComponent (Record Props_ComboboxOptionsImpl)

type Props_ComboboxOptions     = Props_Common     ( children :: Array JSX )
type Props_ComboboxOptionsImpl = Props_CommonImpl ( children :: Array JSX )

comboboxOption
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_ComboboxOption
  => Union attrsImpl attrsImpl_ Props_ComboboxOptionImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
comboboxOption = element (unsafeCoerce comboboxOptionComponent) <<< toNative

foreign import comboboxOptionComponent :: ReactComponent (Record Props_ComboboxOptionImpl)

type Props_ComboboxOption = Props_Common ( children :: Array JSX | Props_ComboboxOptionRow )
type Props_ComboboxOptionRow =
    ( active   :: Boolean
    , disabled :: Boolean
    , selected :: Boolean
    , value    :: String
    )

type Props_ComboboxOptionImpl = Props_CommonImpl ( children :: Array JSX | Props_ComboboxOptionRowImpl )
type Props_ComboboxOptionRowImpl =
    ( active   :: Boolean
    , disabled :: Boolean
    , selected :: Boolean
    , value    :: String
    )

comboboxTarget
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_ComboboxTarget
  => Union attrsImpl attrsImpl_ Props_ComboboxTargetImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
comboboxTarget = element (unsafeCoerce comboboxTargetComponent) <<< toNative

foreign import comboboxTargetComponent :: ReactComponent (Record Props_ComboboxTargetImpl)

type Props_ComboboxTarget =
  Props_Common
    ( children               :: Array JSX
    , refProp                :: String
    , targetType             :: EventsTargetType
    , withAriaAttributes     :: Boolean
    , withExpandedAttribute  :: Boolean
    , withKeyboardNavigation :: Boolean
    )

type Props_ComboboxTargetImpl =
  Props_CommonImpl
    ( children               :: Array JSX
    , refProp                :: String
    , targetType             :: EventsTargetTypeImpl
    , withAriaAttributes     :: Boolean
    , withExpandedAttribute  :: Boolean
    , withKeyboardNavigation :: Boolean
    )

comboboxDropdownTarget
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_ComboboxDropdownTarget
  => Union attrsImpl attrsImpl_ Props_ComboboxDropdownTargetImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
comboboxDropdownTarget = element (unsafeCoerce comboboxDropdownTargetComponent) <<< toNative

comboboxDropdownTarget_ :: Array JSX -> JSX
comboboxDropdownTarget_ children = comboboxDropdownTarget { children }

foreign import comboboxDropdownTargetComponent :: ReactComponent (Record Props_ComboboxDropdownTargetImpl)

type Props_ComboboxDropdownTarget =
  Props_Common
    ( children :: Array JSX
    , refProp  :: String
    )

type Props_ComboboxDropdownTargetImpl =
  Props_CommonImpl
    ( children :: Array JSX
    , refProp  :: String
    )

comboboxEventsTarget
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_ComboboxEventsTarget
  => Union attrsImpl attrsImpl_ Props_ComboboxEventsTargetImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
comboboxEventsTarget = element (unsafeCoerce comboboxEventsTargetComponent) <<< toNative

comboboxEventsTarget_ :: Array JSX -> JSX
comboboxEventsTarget_ children = comboboxEventsTarget { children }

foreign import comboboxEventsTargetComponent :: ReactComponent (Record Props_ComboboxEventsTargetImpl)

type Props_ComboboxEventsTarget =
  Props_Common
    ( children               :: Array JSX
    , refProp                :: String
    , targetType             :: EventsTargetType
    , withAriaAttributes     :: Boolean
    , withExpandedAttribute  :: Boolean
    , withKeyboardNavigation :: Boolean
    )

type Props_ComboboxEventsTargetImpl =
  Props_CommonImpl
    ( children               :: Array JSX
    , refProp                :: String
    , targetType             :: EventsTargetTypeImpl
    , withAriaAttributes     :: Boolean
    , withExpandedAttribute  :: Boolean
    , withKeyboardNavigation :: Boolean
    )

comboboxDropdown
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_ComboboxDropdown
  => Union attrsImpl attrsImpl_ Props_ComboboxDropdownImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
comboboxDropdown = element (unsafeCoerce comboboxDropdownComponent) <<< toNative

comboboxDropdown_ :: Array JSX -> JSX
comboboxDropdown_ children = comboboxDropdown { children }

foreign import comboboxDropdownComponent :: ReactComponent (Record Props_ComboboxDropdownImpl)

type Props_ComboboxDropdown     = Props_Common     ( children :: Array JSX, hidden :: Boolean )
type Props_ComboboxDropdownImpl = Props_CommonImpl ( children :: Array JSX, hidden :: Boolean )

comboboxGroup
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_ComboboxGroup
  => Union attrsImpl attrsImpl_ Props_ComboboxGroupImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
comboboxGroup = element (unsafeCoerce comboboxGroupComponent) <<< toNative

foreign import comboboxGroupComponent :: ReactComponent (Record Props_ComboboxGroupImpl)

type Props_ComboboxGroup     = Props_Common     ( label :: JSX )
type Props_ComboboxGroupImpl = Props_CommonImpl ( label :: JSX )

data EventsTargetType
  = EventsTargetTypeInput
  | EventsTargetTypeButton

type EventsTargetTypeImpl = String

instance ToFFI EventsTargetType EventsTargetTypeImpl where
  toNative = case _ of
    EventsTargetTypeInput  -> "input"
    EventsTargetTypeButton -> "button"

type Props_ComboboxInner =
  ( arrowOffset                 :: Optional Pixels
  , arrowPosition               :: Optional ComboboxArrowPosition
  , arrowRadius                 :: Optional Pixels
  , arrowSize                   :: Optional Pixels
  , disabled                    :: Optional Boolean
  , dropdownPadding             :: Optional Number
  , keepMounted                 :: Optional Boolean
  , middlewares                 :: Optional PopoverMiddlewares
  , offset                      :: Optional Offset
  , onClose                     :: Optional (Effect Unit)
  , onOpen                      :: Optional (Effect Unit)
  , onOptionSubmit              :: Optional (String -> Record Props_ComboboxOptionRow -> Effect Unit)
  , onPositionChange            :: Optional (ValueHandler ComboboxFloatingPosition)
  , position                    :: Optional ComboboxFloatingPosition
  , radius                      :: Optional MantineNumberSize
  , readOnly                    :: Optional Boolean
  , resetSelectionOnOptionHover :: Optional Boolean
  , returnFocus                 :: Optional Boolean
  , shadow                      :: Optional MantineShadow
  , size                        :: Optional MantineSize
  , store                       :: Optional ComboboxStore
  , transitionProps             :: Optional MantineTransitionProps
  , width                       :: Optional ComboboxPopoverWidth
  , withArrow                   :: Optional Boolean
  , withinPortal                :: Optional Boolean
  , zIndex                      :: Optional ZIndex
  )

type Props_ComboboxInnerImpl =
  ( arrowOffset                 :: OptionalImpl PixelsImpl
  , arrowPosition               :: OptionalImpl ComboboxArrowPositionImpl
  , arrowRadius                 :: OptionalImpl PixelsImpl
  , arrowSize                   :: OptionalImpl PixelsImpl
  , disabled                    :: OptionalImpl Boolean
  , dropdownPadding             :: OptionalImpl Number
  , keepMounted                 :: OptionalImpl Boolean
  , middlewares                 :: OptionalImpl PopoverMiddlewaresImpl
  , offset                      :: OptionalImpl OffsetImpl
  , onClose                     :: OptionalImpl (Effect Unit)
  , onOpen                      :: OptionalImpl (Effect Unit)
  , onOptionSubmit              :: OptionalImpl (EffectFn2 String (Record Props_ComboboxOptionRowImpl) Unit)
  , onPositionChange            :: OptionalImpl (ValueHandlerImpl ComboboxFloatingPositionImpl)
  , position                    :: OptionalImpl ComboboxFloatingPositionImpl
  , radius                      :: OptionalImpl MantineNumberSizeImpl
  , readOnly                    :: OptionalImpl Boolean
  , resetSelectionOnOptionHover :: OptionalImpl Boolean
  , returnFocus                 :: OptionalImpl Boolean
  , shadow                      :: OptionalImpl MantineShadowImpl
  , size                        :: OptionalImpl MantineSizeImpl
  , store                       :: OptionalImpl ComboboxStoreImpl
  , transitionProps             :: OptionalImpl MantineTransitionPropsImpl
  , width                       :: OptionalImpl ComboboxPopoverWidthImpl
  , withArrow                   :: OptionalImpl Boolean
  , withinPortal                :: OptionalImpl Boolean
  , zIndex                      :: OptionalImpl ZIndexImpl
  )
