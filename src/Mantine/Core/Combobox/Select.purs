module Mantine.Core.Combobox.Select
  ( select
  , Props_Select
  , Props_SelectImpl

  , multiSelect
  , Props_MultiSelect
  , Props_MultiSelectImpl

  , CheckIconPosition(..)
  , CheckIconPositionImpl
  , ClearablePropsRow
  , ClearablePropsRowImpl
  , SelectItem
  , SelectItemImpl
  , SelectPropsRow
  , SelectPropsRowImpl
  , BaseSelectPropsRow
  , BaseSelectPropsRowImpl

  , module Mantine.Core.Inputs.ClearButtonProps
  ) where

import Mantine.Core.Combobox.Combobox (Props_Combobox, Props_ComboboxImpl)
import Mantine.Core.Inputs.ClearButtonProps (ClearButtonProps, ClearButtonPropsImpl)
import Mantine.Core.Inputs.Input (Props_InputRow, Props_InputRowImpl, WithInputContainer, WithInputContainerImpl)
import Mantine.Core.Prelude

select
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Select
  => Union attrsImpl attrsImpl_ Props_SelectImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
select = element (unsafeCoerce selectComponent) <<< toNative

foreign import selectComponent :: ReactComponent (Record Props_SelectImpl)

type Props_Select =
  Props_Common
    ( allowDeselect :: Boolean
    | SelectPropsRow (Maybe String)
    )

multiSelect
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_MultiSelect
  => Union attrsImpl attrsImpl_ Props_MultiSelectImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
multiSelect = element (unsafeCoerce multiSelectComponent) <<< toNative

foreign import multiSelectComponent :: ReactComponent (Record Props_MultiSelectImpl)

type Props_MultiSelect =
  Props_Common
    ( hidePickedOptions :: Boolean
    , maxValues         :: Int
    | SelectPropsRow (Array String)
    )

type SelectPropsRow items =
    ( checkIconPosition   :: CheckIconPosition
    , nothingFoundMessage :: JSX
    , searchable          :: Boolean
    , withCheckIcon       :: Boolean
    | BaseSelectPropsRow items
    )

type BaseSelectPropsRow items =
    ( comboboxProps             :: Record Props_Combobox
    , data                      :: Array SelectItem
    , defaultDropdownOpened     :: Boolean
    , defaultSearchValue        :: String
    , dropdownOpened            :: Boolean
    , filter                    :: SelectItem -> Boolean
    , limit                     :: Int
    , maxDropdownHeight         :: Pixels
    , onDropdownClose           :: Effect Unit
    , onDropdownOpen            :: Effect Unit
    , onOptionSubmit            :: ValueHandler String
    , onSearchChange            :: ValueHandler String
    , searchValue               :: String
    , selectFirstOptionOnChange :: Boolean
    , withScrollArea            :: Boolean
    | Controlled_ items + ClearablePropsRow + WithInputContainer + Props_InputRow
    )

type ClearablePropsRow rest =
  ( clearable        :: Boolean
  , clearButtonProps :: ClearButtonProps
  | rest
  )

data CheckIconPosition
  = CheckIconPositionLeft
  | CheckIconPositionRight

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

type SelectPropsRowImpl items =
    ( checkIconPosition   :: CheckIconPositionImpl
    , nothingFoundMessage :: JSX
    , searchable          :: Boolean
    , withCheckIcon       :: Boolean
    | BaseSelectPropsRowImpl items
    )

type BaseSelectPropsRowImpl items =
    ( comboboxProps             :: Record Props_ComboboxImpl
    , data                      :: Array SelectItemImpl
    , defaultDropdownOpened     :: Boolean
    , defaultSearchValue        :: String
    , dropdownOpened            :: Boolean
    , filter                    :: SelectItemImpl -> Boolean
    , limit                     :: Number
    , maxDropdownHeight         :: PixelsImpl
    , onDropdownClose           :: Effect Unit
    , onDropdownOpen            :: Effect Unit
    , onOptionSubmit            :: ValueHandlerImpl String
    , onSearchChange            :: ValueHandlerImpl String
    , searchValue               :: String
    , selectFirstOptionOnChange :: Boolean
    , withScrollArea            :: Boolean
    | ControlledImpl_ items + ClearablePropsRowImpl + WithInputContainerImpl + Props_InputRowImpl
    )

type ClearablePropsRowImpl restImpl =
  ( clearable        :: Boolean
  , clearButtonProps :: ClearButtonPropsImpl
  | restImpl
  )

type SelectItemImpl =
  { value    :: String
  , label    :: OptionalImpl String
  , disabled :: OptionalImpl Boolean
  , group    :: OptionalImpl String
  }

type Props_SelectImpl =
  Props_CommonImpl
    ( allowDeselect :: Boolean
    | SelectPropsRowImpl (Nullable String)
    )

type Props_MultiSelectImpl =
  Props_CommonImpl
    ( hidePickedOptions :: Boolean
    , maxValues         :: Number
    | SelectPropsRowImpl (Array String)
    )
