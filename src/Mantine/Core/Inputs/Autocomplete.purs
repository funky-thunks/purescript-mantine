module Mantine.Core.Inputs.Autocomplete
  ( autocomplete
  , AutocompleteProps
  , AutocompleteItem
  , AutocompleteDropdownPosition(..)

  , module Mantine.Core.Inputs.Input
  ) where

import Effect.Uncurried (EffectFn2, mkEffectFn2)
import Mantine.Core.Inputs.Input (InputVariant(..), InputWrapperOrder(..))
import Mantine.Core.Prelude

autocomplete :: (AutocompleteProps -> AutocompleteProps) -> JSX
autocomplete = mkComponent autocompleteComponent autocompleteToImpl defaultAutocompleteProps

foreign import autocompleteComponent :: ReactComponent AutocompletePropsImpl

-- Not supported properties
--   { descriptionProps     :: Record<String, any>
--   , dropdownComponent    :: any
--   , errorProps           :: Record<String, any>
--   , labelProps           :: Record<String, any>
--   , portalProps          :: Omit<PortalProps, "children" | "withinPortal">
--   , positionDependencies :: any[]
--   , rightSectionProps    :: Record<String, any>
--   , shadow               :: MantineShadow
--   , wrapperProps         :: Record<String, any>
--   }

type AutocompleteProps =
  ThemingProps
    ( data                     :: Array AutocompleteItem
    , defaultValue             :: Maybe String
    , description              :: Maybe JSX
    , disabled                 :: Boolean
    , dropdownPosition         :: AutocompleteDropdownPosition
    , error                    :: Maybe JSX
    , filter                   :: Maybe (String -> AutocompleteItem -> Effect Boolean)
    , hoverOnSearchChange      :: Boolean
    , icon                     :: Maybe JSX
    , iconWidth                :: Maybe Pixels
    , id                       :: Maybe String
    , initiallyOpened          :: Boolean
    , inputContainer           :: Maybe (JSX -> JSX)
    , inputWrapperOrder        :: Maybe (Array InputWrapperOrder)
    , itemComponent            :: Maybe (AutocompleteItem -> JSX)
    , label                    :: Maybe JSX
    , limit                    :: Maybe Int
    , maxDropdownHeight        :: Maybe (String |+| Number)
    , nothingFound             :: Maybe JSX
    , onChange                 :: ValueHandler String
    , onDropdownClose          :: Effect Unit
    , onDropdownOpen           :: Effect Unit
    , onItemSubmit             :: ValueHandler AutocompleteItem
    , placeholder              :: Maybe String
    , radius                   :: Maybe MantineNumberSize
    , required                 :: Boolean
    , rightSection             :: Maybe JSX
    , rightSectionWidth        :: Maybe Pixels
    , size                     :: Maybe MantineSize
    , switchDirectionOnFlip    :: Boolean
    , transitionProps          :: MantineTransitionProps
    , value                    :: Maybe String
    , variant                  :: InputVariant
    , withAsterisk             :: Boolean
    , withinPortal             :: Boolean
    , zIndex                   :: Maybe Number
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

data AutocompleteDropdownPosition
  = AutocompleteDropdownPositionBottom
  | AutocompleteDropdownPositionTop
  | AutocompleteDropdownPositionFlip

instance ToFFI AutocompleteDropdownPosition String where
  toNative = case _ of
    AutocompleteDropdownPositionBottom -> "bottom"
    AutocompleteDropdownPositionTop    -> "top"
    AutocompleteDropdownPositionFlip   -> "flip"

instance DefaultValue AutocompleteDropdownPosition where
  defaultValue = AutocompleteDropdownPositionFlip

type AutocompletePropsImpl =
  ThemingPropsImpl
    ( data                     :: Array AutocompleteItem
    , defaultValue             :: Nullable String
    , description              :: Nullable JSX
    , disabled                 :: Boolean
    , dropdownPosition         :: String
    , error                    :: Nullable JSX
    , filter                   :: Nullable (EffectFn2 String AutocompleteItem Boolean)
    , hoverOnSearchChange      :: Boolean
    , icon                     :: Nullable JSX
    , iconWidth                :: Nullable Number
    , id                       :: Nullable String
    , initiallyOpened          :: Boolean
    , inputContainer           :: Nullable (JSX -> JSX)
    , inputWrapperOrder        :: Nullable (Array String)
    , itemComponent            :: Nullable (AutocompleteItem -> JSX)
    , label                    :: Nullable JSX
    , limit                    :: Nullable Number
    , maxDropdownHeight        :: Nullable (String |+| Number)
    , nothingFound             :: Nullable JSX
    , onChange                 :: EffectFn1 String Unit
    , onDropdownClose          :: Effect Unit
    , onDropdownOpen           :: Effect Unit
    , onItemSubmit             :: EffectFn1 AutocompleteItem Unit
    , placeholder              :: Nullable String
    , radius                   :: Nullable MantineNumberSizeImpl
    , required                 :: Boolean
    , rightSection             :: Nullable JSX
    , rightSectionWidth        :: Nullable Number
    , size                     :: Nullable String
    , switchDirectionOnFlip    :: Boolean
    , transitionProps          :: MantineTransitionPropsImpl
    , value                    :: Nullable String
    , variant                  :: String
    , withAsterisk             :: Boolean
    , withinPortal             :: Boolean
    , zIndex                   :: Nullable Number
    )

autocompleteToImpl :: AutocompleteProps -> AutocompletePropsImpl
autocompleteToImpl props =
  let rest = toNative
         <<< delete (Proxy :: Proxy "filter")
         <<< delete (Proxy :: Proxy "maxDropdownHeight")
         <<< delete (Proxy :: Proxy "inputContainer")
         <<< delete (Proxy :: Proxy "itemComponent")
      customProps =
        { filter:            toNullable (mkEffectFn2 <$> props.filter)
        , inputContainer:    toNullable props.inputContainer
        , itemComponent:     toNullable props.itemComponent
        , maxDropdownHeight: toNullable props.maxDropdownHeight
        }
   in customProps `union` rest props
