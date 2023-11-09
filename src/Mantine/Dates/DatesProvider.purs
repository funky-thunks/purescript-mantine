module Mantine.Dates.DatesProvider
  ( datesProvider
  , DatesProviderProps
  , Locale(..)
  , Timezone(..)
  ) where

import Mantine.Dates.Prelude
import Untagged.Union (maybeToUor)

datesProvider :: (DatesProviderProps -> DatesProviderProps) -> JSX
datesProvider = mkComponent datesProviderComponent datesProviderToImpl defaultValue

foreign import datesProviderComponent :: ReactComponent DatesProviderPropsImpl

type DatesProviderProps =
  { children       :: Array JSX
  , locale         :: Maybe Locale
  , timezone       :: Maybe Timezone
  , firstDayOfWeek :: Maybe DayOfWeek
  , weekendDays    :: Maybe (Array DayOfWeek)
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
  { locale         :: UndefinedOr String
  , firstDayOfWeek :: UndefinedOr Number
  , weekendDays    :: UndefinedOr (Array Number)
  , timezone       :: UndefinedOr String
  }

datesProviderToImpl :: DatesProviderProps -> DatesProviderPropsImpl
datesProviderToImpl { children, locale, firstDayOfWeek, weekendDays, timezone } =
  let settings = { locale:         maybeToUor (toNative     <$> locale        )
                 , firstDayOfWeek: maybeToUor (toNative     <$> firstDayOfWeek)
                 , weekendDays:    maybeToUor (map toNative <$> weekendDays   )
                 , timezone:       maybeToUor (toNative     <$> timezone      )
                 }
   in { children, settings }
