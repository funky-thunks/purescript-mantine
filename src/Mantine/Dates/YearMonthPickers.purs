module Mantine.Dates.YearMonthPickers
  ( yearPicker
  , Props_YearPicker

  , monthPicker
  , Props_MonthPicker
  , Props_MonthPickerBase
  , MonthPickerLevel(..)

  , monthPickerInput
  , Props_MonthPickerInput

  , yearPickerInput
  , Props_YearPickerInput

  , Props_YearMonthPicker
  ) where

import Mantine.Dates.Calendar (DatePickerType, DatePickerTypeImpl)
import Mantine.Dates.Prelude

yearPicker
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_YearPicker
  => Union attrsImpl attrsImpl_ Props_YearPickerImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
yearPicker = element (unsafeCoerce yearPickerComponent) <<< toNative

foreign import yearPickerComponent :: ReactComponent (Record Props_YearPickerImpl)

type Props_YearPicker =
  Props_YearMonthPicker
    ( onYearSelect :: ValueHandler JSDate
    , size         :: MantineSize
    )

type Props_YearPickerImpl =
  Props_YearMonthPickerImpl
    ( onYearSelect :: ValueHandlerImpl JSDate
    , size         :: MantineSizeImpl
    )

monthPicker
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_MonthPicker
  => Union attrsImpl attrsImpl_ Props_MonthPickerImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
monthPicker = element (unsafeCoerce monthPickerComponent) <<< toNative

foreign import monthPickerComponent :: ReactComponent (Record Props_MonthPickerImpl)

type Props_MonthPicker =
  Props_MonthPickerBase
    ( onMonthSelect :: ValueHandler JSDate
    , size          :: MantineSize
    )

type Props_MonthPickerImpl =
  Props_MonthPickerBaseImpl
    ( onMonthSelect :: ValueHandlerImpl JSDate
    , size          :: MantineSizeImpl
    )

type Props_MonthPickerBase rest =
  Props_YearMonthPicker
    ( defaultLevel         :: MonthPickerLevel
    , getMonthControlProps :: DateFunction PickerControlProps
    , level                :: MonthPickerLevel
    , maxLevel             :: MonthPickerLevel
    , monthsListFormat     :: String
    , onLevelChange        :: ValueHandler MonthPickerLevel
    , onNextYear           :: ValueHandler JSDate
    , onPreviousYear       :: ValueHandler JSDate
    , yearLabelFormat      :: DateFormat
    | rest
    )

data MonthPickerLevel
  = MonthPickerLevelDecade
  | MonthPickerLevelYear

type MonthPickerLevelImpl = String

instance ToFFI MonthPickerLevel MonthPickerLevelImpl where
  toNative = case _ of
    MonthPickerLevelDecade -> "decade"
    MonthPickerLevelYear   -> "year"

instance FromFFI MonthPickerLevelImpl MonthPickerLevel where
  fromNative = case _ of
    "decade" -> MonthPickerLevelDecade
    "year"   -> MonthPickerLevelYear
    _        -> MonthPickerLevelYear

type Props_MonthPickerBaseImpl rest =
  Props_YearMonthPickerImpl
    ( defaultLevel         :: MonthPickerLevelImpl
    , getMonthControlProps :: DateFunctionImpl PickerControlPropsImpl
    , level                :: MonthPickerLevelImpl
    , maxLevel             :: MonthPickerLevelImpl
    , monthsListFormat     :: String
    , onLevelChange        :: ValueHandlerImpl MonthPickerLevelImpl
    , onNextYear           :: ValueHandlerImpl JSDate
    , onPreviousYear       :: ValueHandlerImpl JSDate
    , yearLabelFormat      :: DateFormatImpl
    | rest
    )

type Props_YearMonthPicker rest =
  DateComponent
    ( allowDeselect          :: Boolean
    , allowSingleDateInRange :: Boolean
    , defaultDate            :: JSDate
    , defaultValue           :: DateValue
    , onChange               :: ValueHandler DateValue
    , type                   :: DatePickerType
    , value                  :: DateValue
    | rest
    )

type Props_YearMonthPickerImpl rest =
  DateComponentImpl
    ( allowDeselect          :: Boolean
    , allowSingleDateInRange :: Boolean
    , defaultDate            :: JSDate
    , defaultValue           :: DateValueImpl
    , onChange               :: ValueHandlerImpl DateValueImpl
    , type                   :: DatePickerTypeImpl
    , value                  :: DateValueImpl
    | rest
    )

monthPickerInput
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_MonthPickerInput
  => Union attrsImpl attrsImpl_ Props_MonthPickerInputImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
monthPickerInput = element (unsafeCoerce monthPickerInputComponent) <<< toNative

foreign import monthPickerInputComponent :: ReactComponent (Record Props_MonthPickerInputImpl)

type Props_MonthPickerInput     = Props_MonthPickerBase     Props_DateInputBase
type Props_MonthPickerInputImpl = Props_MonthPickerBaseImpl Props_DateInputBaseImpl

yearPickerInput
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_YearPickerInput
  => Union attrsImpl attrsImpl_ Props_YearPickerInputImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
yearPickerInput = element (unsafeCoerce yearPickerInputComponent) <<< toNative

foreign import yearPickerInputComponent :: ReactComponent (Record Props_YearPickerInputImpl)

type Props_YearPickerInput     = Props_YearMonthPicker     Props_DateInputBase
type Props_YearPickerInputImpl = Props_YearMonthPickerImpl Props_DateInputBaseImpl
