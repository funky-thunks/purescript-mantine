module Mantine.Core.Combobox.Autocomplete
  ( autocomplete
  , AutocompleteProps
  , AutocompleteItem
  ) where

import Effect.Uncurried (EffectFn2, mkEffectFn2)
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
    ( comboboxProps             :: Maybe ComboboxProps
    , data                      :: Array AutocompleteItem
    , defaultDropdownOpened     :: Maybe Boolean
    , defaultValue              :: Maybe String
    , dropdownOpened            :: Maybe Boolean
    , filter                    :: Maybe (String -> AutocompleteItem -> Effect Boolean)
    , limit                     :: Maybe Int
    , maxDropdownHeight         :: Maybe (String |+| Number)
    , onChange                  :: ValueHandler String
    , onDropdownClose           :: Effect Unit
    , onDropdownOpen            :: Effect Unit
    , onOptionSubmit            :: ValueHandler String
    , selectFirstOptionOnChange :: Boolean
    , value                     :: Maybe String
    , withScrollArea            :: Boolean
    )

defaultAutocompleteProps :: AutocompleteProps
defaultAutocompleteProps =
  defaultThemingProps
    { onDropdownClose: pure unit
    , onDropdownOpen:  pure unit
    }

type AutocompleteItem =
  { value :: String
  }

type AutocompletePropsImpl =
  InputComponentImpl
    ( comboboxProps             :: Nullable ComboboxPropsImpl
    , data                      :: Array AutocompleteItem
    , defaultDropdownOpened     :: Nullable Boolean
    , defaultValue              :: Nullable String
    , dropdownOpened            :: Nullable Boolean
    , filter                    :: Nullable (EffectFn2 String AutocompleteItem Boolean)
    , limit                     :: Nullable Number
    , maxDropdownHeight         :: Nullable (String |+| Number)
    , onChange                  :: EffectFn1 String Unit
    , onDropdownClose           :: Effect Unit
    , onDropdownOpen            :: Effect Unit
    , onOptionSubmit            :: EffectFn1 String Unit
    , selectFirstOptionOnChange :: Boolean
    , value                     :: Nullable String
    , withScrollArea            :: Boolean
    )

autocompleteToImpl :: AutocompleteProps -> AutocompletePropsImpl
autocompleteToImpl props =
  let rest = toNative
         <<< delete (Proxy :: Proxy "filter")
         <<< delete (Proxy :: Proxy "maxDropdownHeight")
      customProps =
        { filter:            toNullable (mkEffectFn2 <$> props.filter)
        , maxDropdownHeight: toNullable props.maxDropdownHeight
        }
   in customProps `union` rest props
