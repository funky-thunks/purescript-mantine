module Mantine.Core.Combobox.Autocomplete
  ( autocomplete
  , AutocompleteProps
  , AutocompleteItem
  ) where

import Mantine.Core.Combobox.Combobox (ComboboxProps, ComboboxPropsImpl)
import Mantine.Core.Inputs.Input (InputComponent, InputComponentImpl)
import Mantine.Core.Prelude

autocomplete :: (AutocompleteProps -> AutocompleteProps) -> JSX
autocomplete = mkComponent autocompleteComponent autocompleteToImpl defaultAutocompleteProps

foreign import autocompleteComponent :: ReactComponent AutocompletePropsImpl

-- Not supported properties
--   { dropdownComponent    :: any
--   , portalProps          :: Omit<PortalProps, "children" | "withinPortal">
--   , positionDependencies :: any[]
--   }

type AutocompleteProps =
  InputComponent
    ( comboboxProps             :: Optional ComboboxProps
    , data                      :: Array AutocompleteItem
    , defaultDropdownOpened     :: Optional Boolean
    , dropdownOpened            :: Optional Boolean
    , filter                    :: Optional (String -> AutocompleteItem -> Effect Boolean)
    , limit                     :: Optional Int
    , maxDropdownHeight         :: Optional (String |+| Number)
    , onDropdownClose           :: Effect Unit
    , onDropdownOpen            :: Effect Unit
    , onOptionSubmit            :: ValueHandler String
    , selectFirstOptionOnChange :: Boolean
    , withScrollArea            :: Boolean
    | Controlled String
    )

defaultAutocompleteProps :: AutocompleteProps
defaultAutocompleteProps =
  defaultMantineComponent
    { onDropdownClose: pure unit
    , onDropdownOpen:  pure unit
    }

type AutocompleteItem =
  { value :: String
  }

type AutocompletePropsImpl =
  InputComponentImpl
    ( comboboxProps             :: OptionalImpl ComboboxPropsImpl
    , data                      :: Array AutocompleteItem
    , defaultDropdownOpened     :: OptionalImpl Boolean
    , dropdownOpened            :: OptionalImpl Boolean
    , filter                    :: OptionalImpl (EffectFn2 String AutocompleteItem Boolean)
    , limit                     :: OptionalImpl Number
    , maxDropdownHeight         :: OptionalImpl (String |+| Number)
    , onDropdownClose           :: Effect Unit
    , onDropdownOpen            :: Effect Unit
    , onOptionSubmit            :: ValueHandlerImpl String
    , selectFirstOptionOnChange :: Boolean
    , withScrollArea            :: Boolean
    | ControlledImpl String
    )

autocompleteToImpl :: AutocompleteProps -> AutocompletePropsImpl
autocompleteToImpl props =
  let rest = toNative
         <<< delete (Proxy :: Proxy "maxDropdownHeight")
      customProps =
        { maxDropdownHeight: toOptionalImpl props.maxDropdownHeight
        }
   in customProps `union` rest props
