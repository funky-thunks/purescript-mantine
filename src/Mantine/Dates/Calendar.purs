module Mantine.Dates.Calendar
  ( calendar
  , CalendarProps

  , datePicker
  , DatePickerProps
  , DatePickerProps_
  , DatePickerType

  , dateInput
  , DateInputProps

  , datePickerInput
  , DatePickerInputProps

  , dateTimePicker
  , DateTimePickerProps

  , timeInput
  , TimeInputProps

  , DatePickerLevel1Component
  , DatePickerLevel2Component
  , DatePickerLevel3Component
  , DatePickerTypeImpl
  ) where

import Mantine.Core.Buttons.ActionIcon (ActionIconProps, ActionIconPropsImpl, actionIconToImpl)
import Mantine.Core.Inputs.Input (InputComponent, InputComponentImpl)
import Mantine.Core.Overlays.Hovering (PopoverProps, PopoverPropsImpl)
import Mantine.Core.Overlays.Modal (SubModalProps, SubModalPropsImpl)
import Mantine.Dates.Prelude
import Web.UIEvent.MouseEvent (MouseEvent)

calendar :: (CalendarProps -> CalendarProps) -> JSX
calendar = mkTrivialComponent calendarComponent

foreign import calendarComponent :: ReactComponent CalendarPropsImpl

type CalendarProps =
  DatePickerLevel3Component
    ( minLevel :: CalendarLevel
    , size     :: Maybe MantineSize
    , static   :: Boolean
    )

type CalendarPropsImpl =
  DatePickerLevel3ComponentImpl
    ( minLevel :: CalendarLevelImpl
    , size     :: Nullable MantineSizeImpl
    , static   :: Boolean
    )

datePicker :: (DatePickerProps -> DatePickerProps) -> JSX
datePicker = mkComponentWithDefault datePickerComponent defaultDatePickerProps

foreign import datePickerComponent :: ReactComponent DatePickerPropsImpl

type DatePickerProps = DatePickerProps_ (size :: Maybe MantineSize)
type DatePickerProps_ rest =
  DatePickerLevel3Component
    ( allowDeselect          :: Boolean
    , allowSingleDateInRange :: Boolean
    , type                   :: DatePickerType
    | Controlled_ DateValue rest
    )

defaultDatePickerProps :: DatePickerProps
defaultDatePickerProps = defaultMantineComponent { allowDeselect: true }

data DatePickerType
  = DatePickerTypeDefault
  | DatePickerTypeRange
  | DatePickerTypeMultiple

instance DefaultValue DatePickerType where defaultValue = DatePickerTypeDefault

type DatePickerTypeImpl = String

instance ToFFI DatePickerType DatePickerTypeImpl where
  toNative = case _ of
    DatePickerTypeDefault  -> "default"
    DatePickerTypeRange    -> "range"
    DatePickerTypeMultiple -> "multiple"

type DatePickerPropsImpl = DatePickerPropsImpl_ (size :: Nullable MantineSizeImpl)
type DatePickerPropsImpl_ rest =
  DatePickerLevel3ComponentImpl
    ( allowDeselect          :: Boolean
    , allowSingleDateInRange :: Boolean
    , type                   :: DatePickerTypeImpl
    | ControlledImpl_ DateValueImpl rest
    )

dateInput :: (DateInputProps -> DateInputProps) -> JSX
dateInput = mkComponent dateInputComponent dateInputToImpl defaultDateInputProps

foreign import dateInputComponent :: ReactComponent DateInputPropsImpl

-- Not supported properties
--   { getDayProps :: Maybe (JSDate -> Omit<Partial<DayProps>, "classNames" | "styles" | "vars">)
--   }

type DateInputProps =
  DatePickerLevel1Component
    ( allowDeselect    :: Boolean
    , dateParser       :: Maybe (String -> Maybe JSDate)
    , defaultDate      :: Maybe JSDate
    , fixOnBlur        :: Boolean
    , maxLevel         :: Maybe CalendarLevel
    , nextDisabled     :: Boolean
    , onLevelClick     :: Effect Unit
    , onNext           :: Effect Unit
    , onPrevious       :: Effect Unit
    , preserveTime     :: Boolean
    , previousDisabled :: Boolean
    , withNext         :: Boolean
    , withPrevious     :: Boolean
    | Controlled_ DateValue + DateInputBaseProps
    )

defaultDateInputProps :: DateInputProps
defaultDateInputProps =
  defaultMantineComponent
    { allowDeselect:    true
    , fixOnBlur:        true
    , nextDisabled:     true
    , onLevelClick:     pure unit
    , onNext:           pure unit
    , onPrevious:       pure unit
    , preserveTime:     true
    , previousDisabled: true
    , withNext:         true
    , withPrevious:     true
    }

type DateInputPropsImpl =
  DatePickerLevel1ComponentImpl
    ( allowDeselect    :: Boolean
    , dateParser       :: Nullable (String -> Nullable JSDate)
    , defaultDate      :: Nullable JSDate
    , fixOnBlur        :: Boolean
    , maxLevel         :: Nullable CalendarLevelImpl
    , nextDisabled     :: Boolean
    , onLevelClick     :: Effect Unit
    , onNext           :: Effect Unit
    , onPrevious       :: Effect Unit
    , preserveTime     :: Boolean
    , previousDisabled :: Boolean
    , withNext         :: Boolean
    , withPrevious     :: Boolean
    | ControlledImpl_ DateValueImpl + DateInputBasePropsImpl
    )

dateInputToImpl :: DateInputProps -> DateInputPropsImpl
dateInputToImpl props =
  let rest = toNative <<< delete (Proxy :: Proxy "clearable")
                      <<< delete (Proxy :: Proxy "dateParser")
      dateParser = toNullable ((\f -> f >>> toNullable) <$> props.dateParser)
   in { dateParser } `union` toNative props.clearable `union` rest props

datePickerInput :: (DatePickerInputProps -> DatePickerInputProps) -> JSX
datePickerInput = mkComponent datePickerInputComponent datePickerInputToImpl defaultDatePickerInputProps

foreign import datePickerInputComponent :: ReactComponent DatePickerInputPropsImpl

type DatePickerInputProps     = DatePickerProps_     DateInputBaseProps
type DatePickerInputPropsImpl = DatePickerPropsImpl_ DateInputBasePropsImpl

defaultDatePickerInputProps :: DatePickerInputProps
defaultDatePickerInputProps = defaultMantineComponent { allowDeselect: true }

datePickerInputToImpl :: DatePickerInputProps -> DatePickerInputPropsImpl
datePickerInputToImpl props =
  let rest = toNative <<< delete (Proxy :: Proxy "clearable")
   in toNative props.clearable `union` rest props

dateTimePicker :: (DateTimePickerProps -> DateTimePickerProps) -> JSX
dateTimePicker = mkComponent dateTimePickerComponent dateTimePickerToImpl defaultMantineComponent_

foreign import dateTimePickerComponent :: ReactComponent DateTimePickerPropsImpl

-- Not supported properties
--   { getDayProps :: Maybe (JSDate -> Omit<Partial<DayProps>, "classNames" | "styles" | "vars">)
--   }

type DateTimePickerProps =
  DatePickerLevel2Component
    ( dropdownType      :: Maybe DropdownType
    , labelSeparator    :: Maybe String
    , modalProps        :: Maybe SubModalProps
    , popoverProps      :: Maybe PopoverProps
    , readOnly          :: Boolean
    , sortDates         :: Boolean
    , submitButtonProps :: Maybe ActionIconProps
    , timeInputProps    :: Maybe TimeInputProps
    , valueFormat       :: Maybe String
    , withSeconds       :: Boolean
    | Controlled_ DateValue + InputProps
    )

type DateTimePickerPropsImpl =
  DatePickerLevel2ComponentImpl
    ( dropdownType      :: Nullable DropdownTypeImpl
    , labelSeparator    :: Nullable String
    , modalProps        :: Nullable SubModalPropsImpl
    , popoverProps      :: Nullable PopoverPropsImpl
    , readOnly          :: Boolean
    , sortDates         :: Boolean
    , submitButtonProps :: Nullable ActionIconPropsImpl
    , timeInputProps    :: Nullable TimeInputPropsImpl
    , valueFormat       :: Nullable String
    , withSeconds       :: Boolean
    | ControlledImpl_ DateValueImpl + InputPropsImpl
    )

dateTimePickerToImpl :: DateTimePickerProps -> DateTimePickerPropsImpl
dateTimePickerToImpl props =
  let rest = toNative <<< delete (Proxy :: Proxy "clearable")
                      <<< delete (Proxy :: Proxy "submitButtonProps")
      submitButtonProps = toNullable (actionIconToImpl <$> props.submitButtonProps)
   in toNative props.clearable `union` { submitButtonProps } `union` rest props

type DatePickerLevel3Component rest =
  DatePickerLevel2Component
    ( defaultDate       :: Maybe JSDate
    , maxLevel          :: Maybe CalendarLevel
    , onMonthMouseEnter :: Maybe (MouseEvent -> JSDate -> Effect Unit)
    , onYearMouseEnter  :: Maybe (MouseEvent -> JSDate -> Effect Unit)
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
    ( defaultLevel         :: Maybe CalendarLevel
    , excludeDate          :: Maybe (DateFunction Boolean)
    , firstDayOfWeek       :: Maybe DayOfWeek
    , getDayAriaLabel      :: Maybe (DateFunction String)
    , getMonthControlProps :: Maybe (DateFunction PickerControlProps)
    , hasNextLevel         :: Boolean
    , hideOutsideDates     :: Boolean
    , hideWeekdays         :: Boolean
    , level                :: Maybe CalendarLevel
    , monthLabelFormat     :: Maybe DateFormat
    , monthsListFormat     :: Maybe String
    , nextIcon             :: Maybe JSX
    , nextLabel            :: Maybe String
    , onLevelChange        :: ValueHandler CalendarLevel
    , onNextMonth          :: ValueHandler JSDate
    , onNextYear           :: ValueHandler JSDate
    , onPreviousMonth      :: ValueHandler JSDate
    , onPreviousYear       :: ValueHandler JSDate
    , previousIcon         :: Maybe JSX
    , previousLabel        :: String
    , renderDay            :: Maybe (DateFunction JSX)
    , weekdayFormat        :: Maybe DateFormat
    , weekendDays          :: Maybe (Array DayOfWeek)
    , yearLabelFormat      :: Maybe DateFormat
    | rest
    )

type DatePickerLevel3ComponentImpl rest =
  DatePickerLevel2ComponentImpl
    ( defaultDate       :: Nullable JSDate
    , maxLevel          :: Nullable CalendarLevelImpl
    , onMonthMouseEnter :: Nullable (EffectFn2 MouseEvent JSDate Unit)
    , onYearMouseEnter  :: Nullable (EffectFn2 MouseEvent JSDate Unit)
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
    ( defaultLevel         :: Nullable CalendarLevelImpl
    , excludeDate          :: Nullable (DateFunctionImpl Boolean)
    , firstDayOfWeek       :: Nullable DayOfWeekImpl
    , getDayAriaLabel      :: Nullable (DateFunctionImpl String)
    , getMonthControlProps :: Nullable (DateFunctionImpl PickerControlPropsImpl)
    , hasNextLevel         :: Boolean
    , hideOutsideDates     :: Boolean
    , hideWeekdays         :: Boolean
    , level                :: Nullable CalendarLevelImpl
    , monthLabelFormat     :: Nullable DateFormatImpl
    , monthsListFormat     :: Nullable String
    , nextIcon             :: Nullable JSX
    , nextLabel            :: Nullable String
    , onLevelChange        :: ValueHandlerImpl CalendarLevelImpl
    , onNextMonth          :: ValueHandlerImpl JSDate
    , onNextYear           :: ValueHandlerImpl JSDate
    , onPreviousMonth      :: ValueHandlerImpl JSDate
    , onPreviousYear       :: ValueHandlerImpl JSDate
    , previousIcon         :: Nullable JSX
    , previousLabel        :: String
    , renderDay            :: Nullable (DateFunctionImpl JSX)
    , weekdayFormat        :: Nullable DateFormatImpl
    , weekendDays          :: Nullable (Array DayOfWeekImpl)
    , yearLabelFormat      :: Nullable DateFormatImpl
    | rest
    )

timeInput :: (TimeInputProps -> TimeInputProps) -> JSX
timeInput = mkTrivialComponent timeInputComponent

foreign import timeInputComponent :: ReactComponent TimeInputPropsImpl

type TimeInputProps     = InputComponent     (withSeconds :: Boolean)
type TimeInputPropsImpl = InputComponentImpl (withSeconds :: Boolean)
