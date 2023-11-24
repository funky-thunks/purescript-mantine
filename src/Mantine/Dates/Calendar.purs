module Mantine.Dates.Calendar
  ( calendar
  , Props_Calendar
  , Props_CalendarImpl

  , datePicker
  , Props_DatePicker
  , Props_DatePickerImpl
  , Props_DatePicker_
  , Props_DatePickerImpl_
  , DatePickerType
  , DatePickerTypeImpl

  , dateInput
  , Props_DateInput
  , Props_DateInputImpl

  , datePickerInput
  , Props_DatePickerInput
  , Props_DatePickerInputImpl

  , dateTimePicker
  , Props_DateTimePicker
  , Props_DateTimePickerImpl

  , timeInput
  , Props_TimeInput
  , Props_TimeInputImpl

  , DatePickerLevel1Component
  , DatePickerLevel1ComponentImpl
  , DatePickerLevel2Component
  , DatePickerLevel2ComponentImpl
  , DatePickerLevel3Component
  , DatePickerLevel3ComponentImpl
  ) where

import Mantine.Core.Buttons.ActionIcon (Props_ActionIconInner, Props_ActionIconInnerImpl)
import Mantine.Core.Inputs.Input (Props_InputComponent, Props_InputComponentImpl)
import Mantine.Core.Overlays.Hovering (Props_Popover, Props_PopoverImpl)
import Mantine.Core.Overlays.Modal (Props_SubModal, Props_SubModalImpl)
import Mantine.Dates.Prelude
import Web.UIEvent.MouseEvent (MouseEvent)

calendar
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Calendar
  => Union attrsImpl attrsImpl_ Props_CalendarImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
calendar = element (unsafeCoerce calendarComponent) <<< toNative

foreign import calendarComponent :: ReactComponent (Record Props_CalendarImpl)

type Props_Calendar =
  DatePickerLevel3Component
    ( minLevel :: CalendarLevel
    , size     :: MantineSize
    , static   :: Boolean
    )

type Props_CalendarImpl =
  DatePickerLevel3ComponentImpl
    ( minLevel :: CalendarLevelImpl
    , size     :: MantineSizeImpl
    , static   :: Boolean
    )

datePicker
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_DatePicker
  => Union attrsImpl attrsImpl_ Props_DatePickerImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
datePicker = element (unsafeCoerce datePickerComponent) <<< toNative

foreign import datePickerComponent :: ReactComponent (Record Props_DatePickerImpl)

type Props_DatePicker = Props_DatePicker_ (size :: MantineSize)
type Props_DatePicker_ rest =
  DatePickerLevel3Component
    ( allowDeselect          :: Boolean
    , allowSingleDateInRange :: Boolean
    , type                   :: DatePickerType
    | Controlled_ DateValue rest
    )

data DatePickerType
  = DatePickerTypeDefault
  | DatePickerTypeRange
  | DatePickerTypeMultiple

type DatePickerTypeImpl = String

instance ToFFI DatePickerType DatePickerTypeImpl where
  toNative = case _ of
    DatePickerTypeDefault  -> "default"
    DatePickerTypeRange    -> "range"
    DatePickerTypeMultiple -> "multiple"

type Props_DatePickerImpl = Props_DatePickerImpl_ (size :: MantineSizeImpl)
type Props_DatePickerImpl_ rest =
  DatePickerLevel3ComponentImpl
    ( allowDeselect          :: Boolean
    , allowSingleDateInRange :: Boolean
    , type                   :: DatePickerTypeImpl
    | ControlledImpl_ DateValueImpl rest
    )

dateInput
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_DateInput
  => Union attrsImpl attrsImpl_ Props_DateInputImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
dateInput = element (unsafeCoerce dateInputComponent) <<< toNative

foreign import dateInputComponent :: ReactComponent (Record Props_DateInputImpl)

-- Not supported properties
--   { getDayProps :: Maybe (JSDate -> Omit<Partial<DayProps>, "classNames" | "styles" | "vars">)
--   }

type Props_DateInput =
  DatePickerLevel1Component
    ( allowDeselect    :: Boolean
    , dateParser       :: String -> Maybe JSDate
    , defaultDate      :: JSDate
    , fixOnBlur        :: Boolean
    , maxLevel         :: CalendarLevel
    , nextDisabled     :: Boolean
    , onLevelClick     :: Effect Unit
    , onNext           :: Effect Unit
    , onPrevious       :: Effect Unit
    , preserveTime     :: Boolean
    , previousDisabled :: Boolean
    , withNext         :: Boolean
    , withPrevious     :: Boolean
    | Controlled_ DateValue + Props_DateInputBase
    )

type Props_DateInputImpl =
  DatePickerLevel1ComponentImpl
    ( allowDeselect    :: Boolean
    , dateParser       :: String -> Nullable JSDate
    , defaultDate      :: JSDate
    , fixOnBlur        :: Boolean
    , maxLevel         :: CalendarLevelImpl
    , nextDisabled     :: Boolean
    , onLevelClick     :: Effect Unit
    , onNext           :: Effect Unit
    , onPrevious       :: Effect Unit
    , preserveTime     :: Boolean
    , previousDisabled :: Boolean
    , withNext         :: Boolean
    , withPrevious     :: Boolean
    | ControlledImpl_ DateValueImpl + Props_DateInputBaseImpl
    )

datePickerInput
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_DatePickerInput
  => Union attrsImpl attrsImpl_ Props_DatePickerInputImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
datePickerInput = element (unsafeCoerce datePickerInputComponent) <<< toNative

foreign import datePickerInputComponent :: ReactComponent (Record Props_DatePickerInputImpl)

type Props_DatePickerInput     = Props_DatePicker_     Props_DateInputBase
type Props_DatePickerInputImpl = Props_DatePickerImpl_ Props_DateInputBaseImpl

dateTimePicker
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_DateTimePicker
  => Union attrsImpl attrsImpl_ Props_DateTimePickerImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
dateTimePicker = element (unsafeCoerce dateTimePickerComponent) <<< toNative

foreign import dateTimePickerComponent :: ReactComponent (Record Props_DateTimePickerImpl)

-- Not supported properties
--   { getDayProps :: Maybe (JSDate -> Omit<Partial<DayProps>, "classNames" | "styles" | "vars">)
--   }

type Props_DateTimePicker =
  DatePickerLevel2Component
    ( dropdownType      :: DropdownType
    , labelSeparator    :: String
    , modalProps        :: Record Props_SubModal
    , popoverProps      :: Record Props_Popover
    , sortDates         :: Boolean
    , submitButtonProps :: Record Props_ActionIconInner
    , timeInputProps    :: Record Props_TimeInput
    , valueFormat       :: String
    , withSeconds       :: Boolean
    | Controlled_ DateValue + InputProps
    )

type Props_DateTimePickerImpl =
  DatePickerLevel2ComponentImpl
    ( dropdownType      :: DropdownTypeImpl
    , labelSeparator    :: String
    , modalProps        :: Record Props_SubModalImpl
    , popoverProps      :: Record Props_PopoverImpl
    , sortDates         :: Boolean
    , submitButtonProps :: Record Props_ActionIconInnerImpl
    , timeInputProps    :: Record Props_TimeInputImpl
    , valueFormat       :: String
    , withSeconds       :: Boolean
    | ControlledImpl_ DateValueImpl + InputPropsImpl
    )

type DatePickerLevel3Component rest =
  DatePickerLevel2Component
    ( defaultDate       :: JSDate
    , maxLevel          :: CalendarLevel
    , onMonthMouseEnter :: MouseEvent -> JSDate -> Effect Unit
    , onYearMouseEnter  :: MouseEvent -> JSDate -> Effect Unit
    | rest
    )

type DatePickerLevel2Component rest =
  DatePickerLevel1Component
    ( onMonthSelect :: ValueHandler JSDate
    , onYearSelect  :: ValueHandler JSDate
    | rest
    )

type DatePickerLevel1Component rest =
  DateComponent
    ( defaultLevel         :: CalendarLevel
    , excludeDate          :: DateFunction Boolean
    , firstDayOfWeek       :: DayOfWeek
    , getDayAriaLabel      :: DateFunction String
    , getMonthControlProps :: DateFunction PickerControlProps
    , hasNextLevel         :: Boolean
    , hideOutsideDates     :: Boolean
    , hideWeekdays         :: Boolean
    , level                :: CalendarLevel
    , monthLabelFormat     :: DateFormat
    , monthsListFormat     :: String
    , nextIcon             :: JSX
    , nextLabel            :: String
    , onLevelChange        :: ValueHandler CalendarLevel
    , onNextMonth          :: ValueHandler JSDate
    , onNextYear           :: ValueHandler JSDate
    , onPreviousMonth      :: ValueHandler JSDate
    , onPreviousYear       :: ValueHandler JSDate
    , previousIcon         :: JSX
    , previousLabel        :: String
    , renderDay            :: DateFunction JSX
    , weekdayFormat        :: DateFormat
    , weekendDays          :: Array DayOfWeek
    , yearLabelFormat      :: DateFormat
    | rest
    )

type DatePickerLevel3ComponentImpl rest =
  DatePickerLevel2ComponentImpl
    ( defaultDate       :: JSDate
    , maxLevel          :: CalendarLevelImpl
    , onMonthMouseEnter :: EffectFn2 MouseEvent JSDate Unit
    , onYearMouseEnter  :: EffectFn2 MouseEvent JSDate Unit
    | rest
    )

type DatePickerLevel2ComponentImpl rest =
  DatePickerLevel1ComponentImpl
    ( onMonthSelect :: ValueHandlerImpl JSDate
    , onYearSelect  :: ValueHandlerImpl JSDate
    | rest
    )

type DatePickerLevel1ComponentImpl rest =
  DateComponentImpl
    ( defaultLevel         :: CalendarLevelImpl
    , excludeDate          :: DateFunctionImpl Boolean
    , firstDayOfWeek       :: DayOfWeekImpl
    , getDayAriaLabel      :: DateFunctionImpl String
    , getMonthControlProps :: DateFunctionImpl PickerControlPropsImpl
    , hasNextLevel         :: Boolean
    , hideOutsideDates     :: Boolean
    , hideWeekdays         :: Boolean
    , level                :: CalendarLevelImpl
    , monthLabelFormat     :: DateFormatImpl
    , monthsListFormat     :: String
    , nextIcon             :: JSX
    , nextLabel            :: String
    , onLevelChange        :: ValueHandlerImpl CalendarLevelImpl
    , onNextMonth          :: ValueHandlerImpl JSDate
    , onNextYear           :: ValueHandlerImpl JSDate
    , onPreviousMonth      :: ValueHandlerImpl JSDate
    , onPreviousYear       :: ValueHandlerImpl JSDate
    , previousIcon         :: JSX
    , previousLabel        :: String
    , renderDay            :: DateFunctionImpl JSX
    , weekdayFormat        :: DateFormatImpl
    , weekendDays          :: Array DayOfWeekImpl
    , yearLabelFormat      :: DateFormatImpl
    | rest
    )

timeInput
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_TimeInput
  => Union attrsImpl attrsImpl_ Props_TimeInputImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
timeInput = element (unsafeCoerce timeInputComponent) <<< toNative

foreign import timeInputComponent :: ReactComponent (Record Props_TimeInputImpl)

type Props_TimeInput     = Props_InputComponent     (withSeconds :: Boolean)
type Props_TimeInputImpl = Props_InputComponentImpl (withSeconds :: Boolean)
