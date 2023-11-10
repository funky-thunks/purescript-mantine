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

  , DateFunction(..)
  , DateFunctionImpl

  , InputProps
  , InputPropsImpl

  , DateInputBaseProps
  , DateInputBasePropsImpl

  , ClearableInputProps_
  , ClearableInputPropsImpl_

  , DateInputBasePropsRow
  , DateInputBasePropsRowImpl
  , DropdownType(..)
  , DropdownTypeImpl
  ) where

import Data.JSDate (JSDate)
import Foreign (Foreign, unsafeToForeign, unsafeFromForeign)
import Mantine.Core.Combobox.Select (ClearablePropsRow, ClearablePropsRowImpl)
import Mantine.Core.Inputs.Input (InputPropsRow_, InputPropsRowImpl_, WithInputContainer, WithInputContainerImpl)
import Mantine.Core.Overlays.Hovering (PopoverProps, PopoverPropsImpl)
import Mantine.Core.Overlays.Modal (SubModalProps, SubModalPropsImpl)
import Mantine.Core.Prelude
import Untagged.Union (toEither1)

type DateComponent rest =
  MantineComponent
    ( ariaLabels          :: Maybe String
    , columnsToScroll     :: Maybe Int
    , date                :: Maybe JSDate
    , decadeLabelFormat   :: Maybe DecadeLabelFormat
    , getYearControlProps :: Maybe (DateFunction PickerControlProps)
    , locale              :: Maybe String
    , maxDate             :: Maybe JSDate
    , minDate             :: Maybe JSDate
    , numberOfColumns     :: Maybe Int
    , onDateChange        :: ValueHandler JSDate
    , onNextDecade        :: ValueHandler JSDate
    , onPreviousDecade    :: ValueHandler JSDate
    , withCellSpacing     :: Maybe Boolean
    , yearsListFormat     :: Maybe String
    | rest
    )

type DateComponentImpl rest =
  MantineComponentImpl
    ( ariaLabels          :: Nullable String
    , columnsToScroll     :: Nullable Number
    , date                :: Nullable JSDate
    , decadeLabelFormat   :: Nullable DecadeLabelFormatImpl
    , getYearControlProps :: Nullable (DateFunctionImpl PickerControlPropsImpl)
    , locale              :: Nullable String
    , maxDate             :: Nullable JSDate
    , minDate             :: Nullable JSDate
    , numberOfColumns     :: Nullable Number
    , onDateChange        :: ValueHandlerImpl JSDate
    , onNextDecade        :: ValueHandlerImpl JSDate
    , onPreviousDecade    :: ValueHandlerImpl JSDate
    , withCellSpacing     :: Nullable Boolean
    , yearsListFormat     :: Nullable String
    | rest
    )

type PickerControlProps =
  { children     :: Array JSX
  , disabled     :: Boolean
  , selected     :: Boolean
  , inRange      :: Boolean
  , firstInRange :: Boolean
  , lastInRange  :: Boolean
  , size         :: Maybe MantineSize
  }

type PickerControlPropsImpl =
  { children     :: Array JSX
  , disabled     :: Boolean
  , selected     :: Boolean
  , inRange      :: Boolean
  , firstInRange :: Boolean
  , lastInRange  :: Boolean
  , size         :: Nullable MantineSizeImpl
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

instance DefaultValue DayOfWeek where defaultValue = Monday

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

instance DefaultValue CalendarLevel where defaultValue = CalendarLevelMonth

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
    _ -> defaultValue

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

newtype DateFunction value = DateFunction (JSDate -> value)

type DateFunctionImpl value = JSDate -> value

instance ToFFI value valueImpl => ToFFI (DateFunction value) (DateFunctionImpl valueImpl) where
  toNative (DateFunction df) = df >>> toNative

type ClearableInputProps_     rest = ClearablePropsRow     + WithInputContainer     + InputPropsRow_     rest
type ClearableInputPropsImpl_ rest = ClearablePropsRowImpl + WithInputContainerImpl + InputPropsRowImpl_ rest

type InputProps      = ClearableInputProps_     ()
type InputPropsImpl  = ClearableInputPropsImpl_ ()

type DateInputBaseProps     = ClearableInputProps_     DateInputBasePropsRow
type DateInputBasePropsImpl = ClearableInputPropsImpl_ DateInputBasePropsRowImpl

type DateInputBasePropsRow =
  ( closeOnChange  :: Boolean
  , dropdownType   :: Maybe DropdownType
  , labelSeparator :: Maybe String
  , modalProps     :: Maybe SubModalProps
  , popoverProps   :: Maybe PopoverProps
  , readOnly       :: Boolean
  , sortDates      :: Boolean
  , valueFormat    :: Maybe String
  )

data DropdownType
  = DropdownTypePopover
  | DropdownTypeModal

type DropdownTypeImpl = String

instance ToFFI DropdownType DropdownTypeImpl where
  toNative = case _ of
    DropdownTypePopover -> "popover"
    DropdownTypeModal   -> "modal"

type DateInputBasePropsRowImpl =
  ( closeOnChange  :: Boolean
  , dropdownType   :: Nullable DropdownTypeImpl
  , labelSeparator :: Nullable String
  , modalProps     :: Nullable SubModalPropsImpl
  , popoverProps   :: Nullable PopoverPropsImpl
  , readOnly       :: Boolean
  , sortDates      :: Boolean
  , valueFormat    :: Nullable String
  )
