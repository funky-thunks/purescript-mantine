module Mantine.Core.Combobox.Autocomplete
  ( autocomplete
  , Props_Autocomplete
  , Props_AutocompleteImpl
  , AutocompleteItem
  ) where

import Mantine.Core.Combobox.Combobox (Props_Combobox, Props_ComboboxImpl)
import Mantine.Core.Inputs.Input (Props_InputComponent, Props_InputComponentImpl)
import Mantine.Core.Prelude

autocomplete
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Autocomplete
  => Union attrsImpl attrsImpl_ Props_AutocompleteImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
autocomplete = element (unsafeCoerce autocompleteComponent) <<< toNative

foreign import autocompleteComponent :: ReactComponent (Record Props_AutocompleteImpl)

-- Not supported properties
--   { dropdownComponent    :: any
--   , portalProps          :: Omit<PortalProps, "children" | "withinPortal">
--   , positionDependencies :: any[]
--   }

type Props_Autocomplete =
  Props_InputComponent
    ( comboboxProps             :: Record Props_Combobox
    , data                      :: Array AutocompleteItem
    , defaultDropdownOpened     :: Boolean
    , dropdownOpened            :: Boolean
    , filter                    :: String -> AutocompleteItem -> Effect Boolean
    , limit                     :: Int
    , maxDropdownHeight         :: Dimension
    , onDropdownClose           :: Effect Unit
    , onDropdownOpen            :: Effect Unit
    , onOptionSubmit            :: ValueHandler String
    , selectFirstOptionOnChange :: Boolean
    , withScrollArea            :: Boolean
    | Controlled String
    )

type AutocompleteItem =
  { value :: String
  }

type Props_AutocompleteImpl =
  Props_InputComponentImpl
    ( comboboxProps             :: Record Props_ComboboxImpl
    , data                      :: Array AutocompleteItem
    , defaultDropdownOpened     :: Boolean
    , dropdownOpened            :: Boolean
    , filter                    :: EffectFn2 String AutocompleteItem Boolean
    , limit                     :: Number
    , maxDropdownHeight         :: DimensionImpl
    , onDropdownClose           :: Effect Unit
    , onDropdownOpen            :: Effect Unit
    , onOptionSubmit            :: ValueHandlerImpl String
    , selectFirstOptionOnChange :: Boolean
    , withScrollArea            :: Boolean
    | ControlledImpl String
    )
