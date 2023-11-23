module Props.Dates where

import Mantine.Dates as MD
import Mantine.FFI (toNative)

calendarProps :: Record MD.Props_Calendar -> Record MD.Props_CalendarImpl
calendarProps = toNative

dateInputProps :: Record MD.Props_DateInput -> Record MD.Props_DateInputImpl
dateInputProps = toNative

datePickerInputProps :: Record MD.Props_DatePickerInput -> Record MD.Props_DatePickerInputImpl
datePickerInputProps = toNative

datePickerProps :: Record MD.Props_DatePicker -> Record MD.Props_DatePickerImpl
datePickerProps = toNative

dateTimePickerProps :: Record MD.Props_DateTimePicker -> Record MD.Props_DateTimePickerImpl
dateTimePickerProps = toNative

datesProviderProps :: Record MD.Props_DatesProvider -> Record MD.Props_DatesProviderImpl
datesProviderProps = toNative

monthPickerInputProps :: Record MD.Props_MonthPickerInput -> Record MD.Props_MonthPickerInputImpl
monthPickerInputProps = toNative

monthPickerProps :: Record MD.Props_MonthPicker -> Record MD.Props_MonthPickerImpl
monthPickerProps = toNative

timeInputProps :: Record MD.Props_TimeInput -> Record MD.Props_TimeInputImpl
timeInputProps = toNative

yearPickerInputProps :: Record MD.Props_YearPickerInput -> Record MD.Props_YearPickerInputImpl
yearPickerInputProps = toNative

yearPickerProps :: Record MD.Props_YearPicker -> Record MD.Props_YearPickerImpl
yearPickerProps = toNative
