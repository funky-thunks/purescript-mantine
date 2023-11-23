module Mantine.Dates.DatesProvider
  ( datesProvider
  , Props_DatesProvider
  , Props_DatesProviderImpl
  , DatesSettings
  , DatesSettingsImpl
  , Locale(..)
  , Timezone(..)
  ) where

import Mantine.Dates.Prelude

datesProvider
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_DatesProvider
  => Union attrsImpl attrsImpl_ Props_DatesProviderImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
datesProvider = element (unsafeCoerce datesProviderComponent) <<< toNative

foreign import datesProviderComponent :: ReactComponent (Record Props_DatesProviderImpl)

type Props_DatesProvider =
  ( children :: Array JSX
  , settings :: DatesSettings
  )

newtype Locale = Locale String

instance ToFFI Locale String where
  toNative (Locale l) = l

newtype Timezone = Timezone String

instance ToFFI Timezone String where
  toNative (Timezone tz) = tz

type Props_DatesProviderImpl =
  ( children :: Array JSX
  , settings :: DatesSettingsImpl
  )

type DatesSettings =
  { locale         :: Optional String
  , firstDayOfWeek :: Optional Number
  , weekendDays    :: Optional (Array Number)
  , timezone       :: Optional String
  }

type DatesSettingsImpl =
  { locale         :: OptionalImpl String
  , firstDayOfWeek :: OptionalImpl Number
  , weekendDays    :: OptionalImpl (Array Number)
  , timezone       :: OptionalImpl String
  }
