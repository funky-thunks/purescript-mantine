module Mantine.Dates.DatesProvider
  ( datesProvider
  , DatesProviderProps
  , Locale(..)
  , Timezone(..)
  ) where

import Mantine.Dates.Prelude

datesProvider :: (DatesProviderProps -> DatesProviderProps) -> JSX
datesProvider = mkComponent datesProviderComponent datesProviderToImpl defaultValue

foreign import datesProviderComponent :: ReactComponent DatesProviderPropsImpl

type DatesProviderProps =
  { children       :: Array JSX
  , locale         :: Optional Locale
  , timezone       :: Optional Timezone
  , firstDayOfWeek :: Optional DayOfWeek
  , weekendDays    :: Optional (Array DayOfWeek)
  }

newtype Locale = Locale String

instance ToFFI Locale String where
  toNative (Locale l) = l

newtype Timezone = Timezone String

instance ToFFI Timezone String where
  toNative (Timezone tz) = tz

type DatesProviderPropsImpl =
  { children :: Array JSX
  , settings :: DatesSettingsImpl
  }

type DatesSettingsImpl =
  { locale         :: OptionalImpl String
  , firstDayOfWeek :: OptionalImpl Number
  , weekendDays    :: OptionalImpl (Array Number)
  , timezone       :: OptionalImpl String
  }

datesProviderToImpl :: DatesProviderProps -> DatesProviderPropsImpl
datesProviderToImpl props@{ children } =
  let settings = toNative (delete (Proxy :: Proxy "children") props)
   in { children, settings }
