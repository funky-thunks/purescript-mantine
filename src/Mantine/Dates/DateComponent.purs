module Mantine.Dates.DateComponent
  ( DateComponent
  , DateComponentImpl

  , PickerControlProps
  , PickerControlPropsImpl

  , DateFormat(..)
  , DateFormatImpl

  , DecadeLabelFormat(..)
  , DecadeLabelFormatImpl

  , DayOfWeek(..)
  , DayOfWeekImpl

  , CalendarLevel(..)
  , CalendarLevelImpl

  , DateValue(..)
  , DateValueImpl

  , DateFunction
  , DateFunctionImpl

  , InputProps
  , InputPropsImpl

  , Props_DateInputBase
  , Props_DateInputBaseImpl

  , ClearableInputProps_
  , ClearableInputPropsImpl_

  , Props_DateInputBaseRow
  , Props_DateInputBaseRowImpl
  , DropdownType(..)
  , DropdownTypeImpl
  ) where

import Data.JSDate (JSDate)
import Foreign (Foreign, unsafeToForeign, unsafeFromForeign)
import Mantine.Core.Combobox.Select (ClearablePropsRow, ClearablePropsRowImpl)
import Mantine.Core.Inputs.Input (Props_InputRow_, Props_InputRowImpl_, WithInputContainer, WithInputContainerImpl)
import Mantine.Core.Overlays.Hovering (Props_Popover, Props_PopoverImpl)
import Mantine.Core.Overlays.Modal (Props_SubModal, Props_SubModalImpl)
import Mantine.Core.Prelude
import Untagged.Union (toEither1)

type DateComponent rest =
  Props_Common
    ( ariaLabels          :: String
    , columnsToScroll     :: Int
    , date                :: JSDate
    , decadeLabelFormat   :: DecadeLabelFormat
    , getYearControlProps :: DateFunction PickerControlProps
    , locale              :: String
    , maxDate             :: JSDate
    , minDate             :: JSDate
    , numberOfColumns     :: Int
    , onDateChange        :: ValueHandler JSDate
    , onNextDecade        :: ValueHandler JSDate
    , onPreviousDecade    :: ValueHandler JSDate
    , withCellSpacing     :: Boolean
    , yearsListFormat     :: String
    | rest
    )

type DateComponentImpl rest =
  Props_CommonImpl
    ( ariaLabels          :: String
    , columnsToScroll     :: Number
    , date                :: JSDate
    , decadeLabelFormat   :: DecadeLabelFormatImpl
    , getYearControlProps :: DateFunctionImpl PickerControlPropsImpl
    , locale              :: String
    , maxDate             :: JSDate
    , minDate             :: JSDate
    , numberOfColumns     :: Number
    , onDateChange        :: ValueHandlerImpl JSDate
    , onNextDecade        :: ValueHandlerImpl JSDate
    , onPreviousDecade    :: ValueHandlerImpl JSDate
    , withCellSpacing     :: Boolean
    , yearsListFormat     :: String
    | rest
    )

type PickerControlProps =
  { children     :: Array JSX
  , disabled     :: Boolean
  , selected     :: Boolean
  , inRange      :: Boolean
  , firstInRange :: Boolean
  , lastInRange  :: Boolean
  , size         :: MantineSize
  }

type PickerControlPropsImpl =
  { children     :: Array JSX
  , disabled     :: Boolean
  , selected     :: Boolean
  , inRange      :: Boolean
  , firstInRange :: Boolean
  , lastInRange  :: Boolean
  , size         :: MantineSizeImpl
  }

data DateFormat
  = DateFormatStatic String
  | DateFormatDynamic (JSDate -> JSX)

type DateFormatImpl = String |+| (JSDate -> JSX)

instance ToFFI DateFormat DateFormatImpl where
  toNative = case _ of
    DateFormatStatic  s -> asOneOf s
    DateFormatDynamic f -> asOneOf f

data DecadeLabelFormat
  = DecadeLabelFormatStatic String
  | DecadeLabelFormatDynamic (JSDate -> JSDate -> JSX)

type DecadeLabelFormatImpl = String |+| (JSDate -> JSDate -> JSX)

instance ToFFI DecadeLabelFormat DecadeLabelFormatImpl where
  toNative = case _ of
    DecadeLabelFormatStatic  s -> asOneOf s
    DecadeLabelFormatDynamic f -> asOneOf f

data DayOfWeek
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday

type DayOfWeekImpl = Number

instance ToFFI DayOfWeek DayOfWeekImpl where
  toNative = toNative <<< case _ of
    Monday     -> 1
    Tuesday    -> 2
    Wednesday  -> 3
    Thursday   -> 4
    Friday     -> 5
    Saturday   -> 6
    Sunday     -> 0

data CalendarLevel
  = CalendarLevelDecade
  | CalendarLevelYear
  | CalendarLevelMonth

type CalendarLevelImpl = String

instance ToFFI CalendarLevel CalendarLevelImpl where
  toNative = case _ of
    CalendarLevelDecade -> "decade"
    CalendarLevelYear   -> "year"
    CalendarLevelMonth  -> "month"

instance FromFFI CalendarLevelImpl CalendarLevel where
  fromNative = case _ of
    "decade" -> CalendarLevelDecade
    "year"   -> CalendarLevelYear
    "month"  -> CalendarLevelMonth
    _        -> CalendarLevelMonth

data DateValue
  = DateValue JSDate
  | DatesRange { from :: JSDate, to :: JSDate }
  | MultipleDates (Array JSDate)

type DateValueImpl = Foreign |+| Array JSDate

instance ToFFI DateValue DateValueImpl where
  toNative = case _ of
    DateValue     d  -> asOneOf (unsafeToForeign d)
    DatesRange    r  -> asOneOf [ r.from, r.to ]
    MultipleDates ds -> asOneOf ds

instance FromFFI DateValueImpl DateValue where
  fromNative =
    let fromArray = case _ of
          [from, to] -> DatesRange { from, to }
          ds         -> MultipleDates ds
        fromDate = DateValue <<< unsafeFromForeign
     in toEither1 >>> either fromDate fromArray

type DateFunction     value = JSDate -> value
type DateFunctionImpl value = JSDate -> value

type ClearableInputProps_     rest = ClearablePropsRow     + WithInputContainer     + Props_InputRow_     rest
type ClearableInputPropsImpl_ rest = ClearablePropsRowImpl + WithInputContainerImpl + Props_InputRowImpl_ rest

type InputProps      = ClearableInputProps_     ()
type InputPropsImpl  = ClearableInputPropsImpl_ ()

type Props_DateInputBase     = ClearableInputProps_     Props_DateInputBaseRow
type Props_DateInputBaseImpl = ClearableInputPropsImpl_ Props_DateInputBaseRowImpl

type Props_DateInputBaseRow =
  ( closeOnChange  :: Boolean
  , dropdownType   :: DropdownType
  , labelSeparator :: String
  , modalProps     :: Record Props_SubModal
  , popoverProps   :: Record Props_Popover
  , sortDates      :: Boolean
  , valueFormat    :: String
  )

data DropdownType
  = DropdownTypePopover
  | DropdownTypeModal

type DropdownTypeImpl = String

instance ToFFI DropdownType DropdownTypeImpl where
  toNative = case _ of
    DropdownTypePopover -> "popover"
    DropdownTypeModal   -> "modal"

type Props_DateInputBaseRowImpl =
  ( closeOnChange  :: Boolean
  , dropdownType   :: DropdownTypeImpl
  , labelSeparator :: String
  , modalProps     :: Record Props_SubModalImpl
  , popoverProps   :: Record Props_PopoverImpl
  , sortDates      :: Boolean
  , valueFormat    :: String
  )
