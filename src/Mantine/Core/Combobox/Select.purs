module Mantine.Core.Combobox.Select
  ( select
  , SelectProps

  , multiSelect
  , MultiSelectProps

  , CheckIconPosition(..)
  , ClearablePropsRow
  , Clearable(..)
  , ClearableImpl
  , SelectItem
  , SelectPropsRow
  , BaseSelectPropsRow

  , module Mantine.Core.Inputs.ClearButtonProps

  , SelectItemImpl
  , BaseSelectPropsRowImpl
  , ClearablePropsRowImpl
  , baseSelectToImpl
  ) where

import Prelude (join)
import Data.Maybe (fromMaybe)
import Effect.Uncurried (mkEffectFn1)
import Mantine.Core.Combobox.Combobox (ComboboxProps, ComboboxPropsImpl)
import Mantine.Core.Inputs.ClearButtonProps (ClearButtonProps, ClearButtonPropsImpl)
import Mantine.Core.Inputs.Input (InputPropsRow, InputPropsRowImpl, WithInputContainer, WithInputContainerImpl)
import Mantine.Core.Prelude
import Prim.Row (class Lacks)
import Prim.RowList (class RowToList)
import Mantine.FFI (class RecordToFFI)

select :: (SelectProps -> SelectProps) -> JSX
select = mkComponent selectComponent selectToImpl defaultMantineComponent_

foreign import selectComponent :: ReactComponent SelectPropsImpl

type SelectProps =
  MantineComponent
    ( allowDeselect :: Optional Boolean
    | SelectPropsRow (Maybe String)
    )

multiSelect :: (MultiSelectProps -> MultiSelectProps) -> JSX
multiSelect = mkComponent multiSelectComponent multiSelectToImpl defaultMantineComponent_

foreign import multiSelectComponent :: ReactComponent MultiSelectPropsImpl

type MultiSelectProps =
  MantineComponent
    ( hidePickedOptions :: Boolean
    , maxValues         :: Optional Int
    | SelectPropsRow (Array String)
    )

type SelectPropsRow items =
    ( checkIconPosition   :: CheckIconPosition
    , nothingFoundMessage :: Optional JSX
    , searchable          :: Boolean
    , withCheckIcon       :: Boolean
    | BaseSelectPropsRow items
    )

type BaseSelectPropsRow items =
    ( comboboxProps             :: Optional ComboboxProps
    , data                      :: Array SelectItem
    , defaultDropdownOpened     :: Optional Boolean
    , defaultSearchValue        :: Optional String
    , defaultValue              :: Optional items
    , dropdownOpened            :: Optional Boolean
    , filter                    :: Optional (SelectItem -> Boolean)
    , limit                     :: Optional Int
    , maxDropdownHeight         :: Optional Pixels
    , onChange                  :: Optional (items -> Effect Unit)
    , onDropdownClose           :: Optional (Effect Unit)
    , onDropdownOpen            :: Optional (Effect Unit)
    , onOptionSubmit            :: ValueHandler String
    , onSearchChange            :: ValueHandler String
    , searchValue               :: Optional String
    , selectFirstOptionOnChange :: Boolean
    , value                     :: Optional items
    , withScrollArea            :: Optional Boolean
    | ClearablePropsRow + WithInputContainer + InputPropsRow
    )

type ClearablePropsRow rest =
  ( clearable :: Clearable
  | rest
  )

data CheckIconPosition
  = CheckIconPositionLeft
  | CheckIconPositionRight

instance DefaultValue CheckIconPosition where
  defaultValue = CheckIconPositionLeft

type CheckIconPositionImpl = String

instance ToFFI CheckIconPosition CheckIconPositionImpl where
  toNative = case _ of
    CheckIconPositionLeft  -> "left"
    CheckIconPositionRight -> "right"

type SelectItem =
  { value    :: String
  , label    :: Optional String
  , disabled :: Optional Boolean
  , group    :: Optional String
  }

data Clearable
  = NotClearable
  | Clearable ClearButtonProps

instance DefaultValue Clearable where defaultValue = NotClearable

type ClearableImpl = { | ClearablePropsRowImpl () }

instance ToFFI Clearable ClearableImpl where
  toNative = toNative <<< case _ of
    Clearable p  -> { clearable: true,  clearButtonProps: Optional (pure p) }
    NotClearable -> { clearable: false, clearButtonProps: Optional  Nothing }

type SelectPropsRowImpl items =
    ( checkIconPosition   :: CheckIconPositionImpl
    , nothingFoundMessage :: OptionalImpl JSX
    , searchable          :: Boolean
    , withCheckIcon       :: Boolean
    | BaseSelectPropsRowImpl items
    )

type BaseSelectPropsRowImpl items =
    ( comboboxProps             :: OptionalImpl ComboboxPropsImpl
    , data                      :: Array SelectItemImpl
    , defaultDropdownOpened     :: OptionalImpl Boolean
    , defaultSearchValue        :: OptionalImpl String
    , defaultValue              :: OptionalImpl items
    , dropdownOpened            :: OptionalImpl Boolean
    , filter                    :: OptionalImpl (SelectItemImpl -> Boolean)
    , limit                     :: OptionalImpl Number
    , maxDropdownHeight         :: OptionalImpl PixelsImpl
    , onChange                  :: OptionalImpl (EffectFn1 (Nullable items) Unit)
    , onDropdownClose           :: OptionalImpl (Effect Unit)
    , onDropdownOpen            :: OptionalImpl (Effect Unit)
    , onOptionSubmit            :: ValueHandlerImpl String
    , onSearchChange            :: ValueHandlerImpl String
    , searchValue               :: OptionalImpl String
    , selectFirstOptionOnChange :: Boolean
    , value                     :: OptionalImpl items
    , withScrollArea            :: OptionalImpl Boolean
    | ClearablePropsRowImpl + WithInputContainerImpl + InputPropsRowImpl
    )

type ClearablePropsRowImpl restImpl =
  ( clearable        :: Boolean
  , clearButtonProps :: OptionalImpl ClearButtonPropsImpl
  | restImpl
  )

type SelectItemImpl =
  { value    :: String
  , label    :: OptionalImpl String
  , disabled :: OptionalImpl Boolean
  , group    :: OptionalImpl String
  }

type SelectPropsImpl =
  MantineComponentImpl
    ( allowDeselect :: OptionalImpl Boolean
    | SelectPropsRowImpl (Nullable String)
    )

selectToImpl :: SelectProps -> SelectPropsImpl
selectToImpl = baseSelectToImpl (\h -> mkEffectFn1 (h <<< join <<< map toMaybe <<< toMaybe))

type MultiSelectPropsImpl =
  MantineComponentImpl
    ( hidePickedOptions :: Boolean
    , maxValues         :: OptionalImpl Number
    | SelectPropsRowImpl (Array String)
    )

multiSelectToImpl :: MultiSelectProps -> MultiSelectPropsImpl
multiSelectToImpl = baseSelectToImpl (\h -> mkEffectFn1 (h <<< fromMaybe [] <<< toMaybe))

baseSelectToImpl :: forall items itemsImpl otherPropsRow otherPropsRowList otherPropsRowImpl
  . RowToList otherPropsRow otherPropsRowList
  => RecordToFFI otherPropsRowList otherPropsRow otherPropsRowImpl
  => Lacks "clearable" otherPropsRow
  => Lacks "filter"    otherPropsRow
  => Lacks "onChange"  otherPropsRow
  => ToFFI items itemsImpl
  => ((items -> Effect Unit) -> (EffectFn1 (Nullable itemsImpl) Unit))
  -> { filter   :: Optional (SelectItem -> Boolean)
     , onChange :: Optional (items  -> Effect Unit)
     | ClearablePropsRow + otherPropsRow
     }
  -> { filter   :: OptionalImpl (SelectItemImpl -> Boolean)
     , onChange :: OptionalImpl (EffectFn1 (Nullable itemsImpl) Unit)
     | ClearablePropsRowImpl + otherPropsRowImpl
     }
baseSelectToImpl onChangeToNative props =
  let otherProps =
        { filter:   toOptionalImpl $ (\f -> f <<< fromNative) <$> props.filter
        , onChange: toOptionalImpl $ onChangeToNative <$> props.onChange
        }

      rest = toNative
         <<< delete (Proxy :: Proxy "clearable")
         <<< delete (Proxy :: Proxy "filter")
         <<< delete (Proxy :: Proxy "onChange")

      clearableProps = toNative props.clearable

   in clearableProps `union` otherProps `union` rest props
