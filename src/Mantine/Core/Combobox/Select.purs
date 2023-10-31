module Mantine.Core.Combobox.Select
  ( select
  , SelectProps

  , multiSelect
  , MultiSelectProps

  , CheckIconPosition(..)
  , ClearablePropsRow
  , SelectClearable(..)
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
select = mkComponent selectComponent selectToImpl defaultThemingProps_

foreign import selectComponent :: ReactComponent SelectPropsImpl

type SelectProps =
  ThemingProps
    ( allowDeselect :: Maybe Boolean
    | SelectPropsRow (Maybe String)
    )

multiSelect :: (MultiSelectProps -> MultiSelectProps) -> JSX
multiSelect = mkComponent multiSelectComponent multiSelectToImpl defaultThemingProps_

foreign import multiSelectComponent :: ReactComponent MultiSelectPropsImpl

type MultiSelectProps =
  ThemingProps
    ( hidePickedOptions :: Boolean
    , maxValues         :: Maybe Int
    | SelectPropsRow (Array String)
    )

type SelectPropsRow items =
    ( checkIconPosition   :: CheckIconPosition
    , nothingFoundMessage :: Maybe JSX
    , searchable          :: Boolean
    , withCheckIcon       :: Boolean
    | BaseSelectPropsRow items
    )

type BaseSelectPropsRow items =
    ( comboboxProps             :: Maybe ComboboxProps
    , data                      :: Array SelectItem
    , defaultDropdownOpened     :: Maybe Boolean
    , defaultSearchValue        :: Maybe String
    , defaultValue              :: Maybe items
    , dropdownOpened            :: Maybe Boolean
    , filter                    :: Maybe (SelectItem -> Boolean)
    , limit                     :: Maybe Int
    , maxDropdownHeight         :: Maybe Pixels
    , onChange                  :: Maybe (items -> Effect Unit)
    , onDropdownClose           :: Maybe (Effect Unit)
    , onDropdownOpen            :: Maybe (Effect Unit)
    , onOptionSubmit            :: ValueHandler String
    , onSearchChange            :: Maybe (String -> Effect Unit)
    , searchValue               :: Maybe String
    , selectFirstOptionOnChange :: Boolean
    , value                     :: Maybe items
    , withScrollArea            :: Maybe Boolean
    | ClearablePropsRow + WithInputContainer + InputPropsRow
    )

type ClearablePropsRow rest =
  ( clearable :: SelectClearable
  | rest
  )

data CheckIconPosition
  = CheckIconPositionLeft
  | CheckIconPositionRight

instance DefaultValue CheckIconPosition where defaultValue = CheckIconPositionLeft

instance ToFFI CheckIconPosition String where
  toNative = case _ of
    CheckIconPositionLeft  -> "left"
    CheckIconPositionRight -> "right"

type SelectItem =
  { value    :: String
  , label    :: Maybe String
  , disabled :: Maybe Boolean
  , group    :: Maybe String
  }

data SelectClearable
  = SelectNotClearable
  | SelectClearable ClearButtonProps

instance DefaultValue SelectClearable where defaultValue = SelectNotClearable

type SelectPropsRowImpl items =
    ( checkIconPosition         :: String
    , nothingFoundMessage       :: Nullable JSX
    , searchable                :: Boolean
    , withCheckIcon             :: Boolean
    | BaseSelectPropsRowImpl items
    )

type BaseSelectPropsRowImpl items =
    ( comboboxProps             :: Nullable ComboboxPropsImpl
    , data                      :: Array SelectItemImpl
    , defaultDropdownOpened     :: Nullable Boolean
    , defaultSearchValue        :: Nullable String
    , defaultValue              :: Nullable items
    , dropdownOpened            :: Nullable Boolean
    , filter                    :: Nullable (SelectItemImpl -> Boolean)
    , limit                     :: Nullable Number
    , maxDropdownHeight         :: Nullable Number
    , onChange                  :: Nullable (EffectFn1 (Nullable items) Unit)
    , onDropdownClose           :: Nullable (Effect Unit)
    , onDropdownOpen            :: Nullable (Effect Unit)
    , onOptionSubmit            :: EffectFn1 String Unit
    , onSearchChange            :: Nullable (EffectFn1 String Unit)
    , searchValue               :: Nullable String
    , selectFirstOptionOnChange :: Boolean
    , value                     :: Nullable items
    , withScrollArea            :: Nullable Boolean
    | ClearablePropsRowImpl + WithInputContainerImpl + InputPropsRowImpl
    )

type ClearablePropsRowImpl restImpl =
  ( clearable        :: Boolean
  , clearButtonProps :: Nullable ClearButtonPropsImpl
  | restImpl
  )

type SelectItemImpl =
  { value    :: String
  , label    :: Nullable String
  , disabled :: Nullable Boolean
  , group    :: Nullable String
  }

type SelectPropsImpl =
  ThemingPropsImpl
    ( allowDeselect :: Nullable Boolean
    | SelectPropsRowImpl (Nullable String)
    )

selectToImpl :: SelectProps -> SelectPropsImpl
selectToImpl = baseSelectToImpl (\h -> mkEffectFn1 (h <<< join <<< map toMaybe <<< toMaybe))

type MultiSelectPropsImpl =
  ThemingPropsImpl
    ( hidePickedOptions :: Boolean
    , maxValues         :: Nullable Number
    | SelectPropsRowImpl (Array String)
    )

multiSelectToImpl :: MultiSelectProps -> MultiSelectPropsImpl
multiSelectToImpl = baseSelectToImpl (\h -> mkEffectFn1 (h <<< fromMaybe [] <<< toMaybe))

baseSelectToImpl :: forall items itemsImpl otherPropsRow otherPropsRowList otherPropsRowImpl
  . RowToList otherPropsRow otherPropsRowList
  => RecordToFFI otherPropsRowList otherPropsRow otherPropsRowImpl
  => Lacks "clearable"      otherPropsRow
  => Lacks "filter"         otherPropsRow
  => Lacks "onChange"       otherPropsRow
  => Lacks "onSearchChange" otherPropsRow
  => ToFFI items itemsImpl
  => ((items -> Effect Unit) -> (EffectFn1 (Nullable itemsImpl) Unit))
  -> { filter         :: Maybe (SelectItem -> Boolean)
     , onChange       :: Maybe (items  -> Effect Unit)
     , onSearchChange :: Maybe (String -> Effect Unit)
     | ClearablePropsRow + otherPropsRow
     }
  -> { filter         :: Nullable (SelectItemImpl -> Boolean)
     , onChange       :: Nullable (EffectFn1 (Nullable itemsImpl) Unit)
     , onSearchChange :: Nullable (EffectFn1  String              Unit)
     | ClearablePropsRowImpl + otherPropsRowImpl
     }
baseSelectToImpl onChangeToNative props =
  let otherProps =
        { filter:         toNullable $ (\f -> f <<< fromNative) <$> props.filter
        , onChange:       toNullable $ onChangeToNative <$> props.onChange
        , onSearchChange: toNullable $ mkEffectFn1 <$> props.onSearchChange
        }

      rest = toNative
         <<< delete (Proxy :: Proxy "clearable")
         <<< delete (Proxy :: Proxy "filter")
         <<< delete (Proxy :: Proxy "onChange")
         <<< delete (Proxy :: Proxy "onSearchChange")

      clearableProps = toNative $ case props.clearable of
        SelectClearable p  -> { clearable: true,  clearButtonProps: pure p  }
        SelectNotClearable -> { clearable: false, clearButtonProps: Nothing }

   in clearableProps `union` otherProps `union` rest props
