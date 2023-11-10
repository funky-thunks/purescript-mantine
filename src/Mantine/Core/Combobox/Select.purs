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
    ( allowDeselect :: Maybe Boolean
    | SelectPropsRow (Maybe String)
    )

multiSelect :: (MultiSelectProps -> MultiSelectProps) -> JSX
multiSelect = mkComponent multiSelectComponent multiSelectToImpl defaultMantineComponent_

foreign import multiSelectComponent :: ReactComponent MultiSelectPropsImpl

type MultiSelectProps =
  MantineComponent
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
    , onSearchChange            :: ValueHandler String
    , searchValue               :: Maybe String
    , selectFirstOptionOnChange :: Boolean
    , value                     :: Maybe items
    , withScrollArea            :: Maybe Boolean
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
  , label    :: Maybe String
  , disabled :: Maybe Boolean
  , group    :: Maybe String
  }

data Clearable
  = NotClearable
  | Clearable ClearButtonProps

instance DefaultValue Clearable where defaultValue = NotClearable

type ClearableImpl = { | ClearablePropsRowImpl () }

instance ToFFI Clearable ClearableImpl where
  toNative = toNative <<< case _ of
    Clearable p  -> { clearable: true,  clearButtonProps: pure p  }
    NotClearable -> { clearable: false, clearButtonProps: Nothing }

type SelectPropsRowImpl items =
    ( checkIconPosition   :: CheckIconPositionImpl
    , nothingFoundMessage :: Nullable JSX
    , searchable          :: Boolean
    , withCheckIcon       :: Boolean
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
    , maxDropdownHeight         :: Nullable PixelsImpl
    , onChange                  :: Nullable (EffectFn1 (Nullable items) Unit)
    , onDropdownClose           :: Nullable (Effect Unit)
    , onDropdownOpen            :: Nullable (Effect Unit)
    , onOptionSubmit            :: ValueHandlerImpl String
    , onSearchChange            :: ValueHandlerImpl String
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
  MantineComponentImpl
    ( allowDeselect :: Nullable Boolean
    | SelectPropsRowImpl (Nullable String)
    )

selectToImpl :: SelectProps -> SelectPropsImpl
selectToImpl = baseSelectToImpl (\h -> mkEffectFn1 (h <<< join <<< map toMaybe <<< toMaybe))

type MultiSelectPropsImpl =
  MantineComponentImpl
    ( hidePickedOptions :: Boolean
    , maxValues         :: Nullable Number
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
  -> { filter         :: Maybe (SelectItem -> Boolean)
     , onChange       :: Maybe (items  -> Effect Unit)
     | ClearablePropsRow + otherPropsRow
     }
  -> { filter         :: Nullable (SelectItemImpl -> Boolean)
     , onChange       :: Nullable (EffectFn1 (Nullable itemsImpl) Unit)
     | ClearablePropsRowImpl + otherPropsRowImpl
     }
baseSelectToImpl onChangeToNative props =
  let otherProps =
        { filter:   toNullable $ (\f -> f <<< fromNative) <$> props.filter
        , onChange: toNullable $ onChangeToNative <$> props.onChange
        }

      rest = toNative
         <<< delete (Proxy :: Proxy "clearable")
         <<< delete (Proxy :: Proxy "filter")
         <<< delete (Proxy :: Proxy "onChange")

      clearableProps = toNative props.clearable

   in clearableProps `union` otherProps `union` rest props
