module Mantine.Dates.YearMonthPickers
  ( yearPicker
  , YearPickerProps

  , monthPicker
  , MonthPickerProps
  , MonthPickerBaseProps
  , MonthPickerLevel(..)

  , monthPickerInput
  , MonthPickerInputProps

  , yearPickerInput
  , YearPickerInputProps

  , YearMonthPickerProps
  ) where

import Mantine.Dates.Calendar (DatePickerType, DatePickerTypeImpl)
import Mantine.Dates.Prelude

yearPicker :: (YearPickerProps -> YearPickerProps) -> JSX
yearPicker = mkTrivialComponent yearPickerComponent

foreign import yearPickerComponent :: ReactComponent YearPickerPropsImpl

type YearPickerProps =
  YearMonthPickerProps
    ( onYearSelect :: ValueHandler JSDate
    , size         :: Maybe MantineSize
    )

type YearPickerPropsImpl =
  YearMonthPickerPropsImpl
    ( onYearSelect :: ValueHandlerImpl JSDate
    , size         :: Nullable MantineSizeImpl
    )

monthPicker :: (MonthPickerProps -> MonthPickerProps) -> JSX
monthPicker = mkTrivialComponent monthPickerComponent

foreign import monthPickerComponent :: ReactComponent MonthPickerPropsImpl

type MonthPickerProps =
  MonthPickerBaseProps
    ( onMonthSelect :: ValueHandler JSDate
    , size          :: Maybe MantineSize
    )

type MonthPickerPropsImpl =
  MonthPickerBasePropsImpl
    ( onMonthSelect :: ValueHandlerImpl JSDate
    , size          :: Nullable MantineSizeImpl
    )

type MonthPickerBaseProps rest =
  YearMonthPickerProps
    ( defaultLevel         :: Maybe MonthPickerLevel
    , getMonthControlProps :: Maybe (DateFunction PickerControlProps)
    , level                :: Maybe MonthPickerLevel
    , maxLevel             :: Maybe MonthPickerLevel
    , monthsListFormat     :: Maybe String
    , onLevelChange        :: ValueHandler MonthPickerLevel
    , onNextYear           :: ValueHandler JSDate
    , onPreviousYear       :: ValueHandler JSDate
    , yearLabelFormat      :: Maybe DateFormat
    | rest
    )

data MonthPickerLevel
  = MonthPickerLevelDecade
  | MonthPickerLevelYear

instance DefaultValue MonthPickerLevel where defaultValue = MonthPickerLevelYear

type MonthPickerLevelImpl = String

instance ToFFI MonthPickerLevel MonthPickerLevelImpl where
  toNative = case _ of
    MonthPickerLevelDecade -> "decade"
    MonthPickerLevelYear   -> "year"

instance FromFFI MonthPickerLevelImpl MonthPickerLevel where
  fromNative = case _ of
    "decade" -> MonthPickerLevelDecade
    "year"   -> MonthPickerLevelYear
    _ -> defaultValue

type MonthPickerBasePropsImpl rest =
  YearMonthPickerPropsImpl
    ( defaultLevel         :: Nullable MonthPickerLevelImpl
    , getMonthControlProps :: Nullable (DateFunctionImpl PickerControlPropsImpl)
    , level                :: Nullable MonthPickerLevelImpl
    , maxLevel             :: Nullable MonthPickerLevelImpl
    , monthsListFormat     :: Nullable String
    , onLevelChange        :: ValueHandlerImpl MonthPickerLevelImpl
    , onNextYear           :: ValueHandlerImpl JSDate
    , onPreviousYear       :: ValueHandlerImpl JSDate
    , yearLabelFormat      :: Nullable DateFormatImpl
    | rest
    )

type YearMonthPickerProps rest =
  DateComponent
    ( allowDeselect          :: Maybe Boolean
    , allowSingleDateInRange :: Maybe Boolean
    , defaultDate            :: Maybe JSDate
    , defaultValue           :: Maybe DateValue
    , onChange               :: ValueHandler DateValue
    , type                   :: DatePickerType
    , value                  :: Maybe DateValue
    | rest
    )

type YearMonthPickerPropsImpl rest =
  DateComponentImpl
    ( allowDeselect          :: Nullable Boolean
    , allowSingleDateInRange :: Nullable Boolean
    , defaultDate            :: Nullable JSDate
    , defaultValue           :: Nullable DateValueImpl
    , onChange               :: ValueHandlerImpl DateValueImpl
    , type                   :: DatePickerTypeImpl
    , value                  :: Nullable DateValueImpl
    | rest
    )

monthPickerInput :: (MonthPickerInputProps -> MonthPickerInputProps) -> JSX
monthPickerInput = mkComponent monthPickerInputComponent monthPickerInputToImpl defaultMantineComponent_

foreign import monthPickerInputComponent :: ReactComponent MonthPickerInputPropsImpl

type MonthPickerInputProps     = MonthPickerBaseProps     DateInputBaseProps
type MonthPickerInputPropsImpl = MonthPickerBasePropsImpl DateInputBasePropsImpl

monthPickerInputToImpl :: MonthPickerInputProps -> MonthPickerInputPropsImpl
monthPickerInputToImpl props =
  let rest = toNative <<< delete (Proxy :: Proxy "clearable")
   in toNative props.clearable `union` rest props

yearPickerInput :: (YearPickerInputProps -> YearPickerInputProps) -> JSX
yearPickerInput = mkComponent yearPickerInputComponent yearPickerInputToImpl defaultMantineComponent_

foreign import yearPickerInputComponent :: ReactComponent YearPickerInputPropsImpl

type YearPickerInputProps     = YearMonthPickerProps     DateInputBaseProps
type YearPickerInputPropsImpl = YearMonthPickerPropsImpl DateInputBasePropsImpl

yearPickerInputToImpl :: YearPickerInputProps -> YearPickerInputPropsImpl
yearPickerInputToImpl props =
  let rest = toNative <<< delete (Proxy :: Proxy "clearable")
   in toNative props.clearable `union` rest props
