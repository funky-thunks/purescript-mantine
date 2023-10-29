module Mantine.Core.Inputs.MultiSelect
  ( multiSelect
  , MultiSelectProps
  , MultiSelectItem
  , MultiSelectClearable(..)
  , MultiSelectCreatable(..)
  , MultiSelectDropdownPosition(..)

  , module Mantine.Core.Inputs.ClearButtonProps
  , module Mantine.Core.Inputs.Input
  ) where

import Data.Maybe (fromMaybe, maybe)
import Effect.Uncurried (mkEffectFn1)
import Mantine.Core.Inputs.ClearButtonProps (ClearButtonProps, ClearButtonPropsImpl)
import Mantine.Core.Inputs.Input (InputVariant(..), InputWrapperOrder(..))
import Mantine.Core.Prelude

multiSelect :: (MultiSelectProps -> MultiSelectProps) -> JSX
multiSelect = mkComponent multiSelectComponent multiSelectToImpl defaultThemingProps_

foreign import multiSelectComponent :: ReactComponent MultiSelectPropsImpl

type MultiSelectProps =
  ThemingProps
    ( clearSearchOnBlur            :: Boolean
    , clearSearchOnChange          :: Boolean
    , clearable                    :: MultiSelectClearable
    , creatable                    :: MultiSelectCreatable
    , data                         :: Array MultiSelectItem
    , defaultValue                 :: Maybe (Array String)
    , description                  :: Maybe JSX
    , disableSelectedItemFiltering :: Boolean
    , disabled                     :: Boolean
    , dropdownPosition             :: MultiSelectDropdownPosition
    , error                        :: Maybe JSX
    , filter                       :: Maybe (MultiSelectItem -> Boolean)
    , hoverOnSearchChange          :: Boolean
    , icon                         :: Maybe JSX
    , iconWidth                    :: Maybe Pixels
    , initiallyOpened              :: Boolean
    , inputContainer               :: Maybe (JSX -> JSX)
    , inputWrapperOrder            :: Maybe (Array InputWrapperOrder)
    , itemComponent                :: Maybe (MultiSelectItem -> JSX)
    , label                        :: Maybe JSX
    , limit                        :: Maybe Int
    , maxDropdownHeight            :: Maybe Pixels
    , maxSelectedValues            :: Maybe Int
    , nothingFound                 :: Maybe JSX
    , onChange                     :: Maybe (Array String -> Effect Unit)
    , onDropdownClose              :: Maybe (Effect Unit)
    , onDropdownOpen               :: Maybe (Effect Unit)
    , onSearchChange               :: Maybe (String -> Effect Unit)
    , radius                       :: Maybe MantineNumberSize
    , required                     :: Boolean
    , rightSection                 :: Maybe JSX
    , rightSectionWidth            :: Maybe Pixels
    , searchValue                  :: Maybe String
    , searchable                   :: Boolean
    , selectOnBlur                 :: Boolean
    , size                         :: Maybe MantineSize
    , switchDirectionOnFlip        :: Boolean
    , transitionProps              :: MantineTransitionProps
    , value                        :: Maybe (Array String)
    , valueComponent               :: Maybe (MultiSelectItem -> JSX)
    , variant                      :: InputVariant
    , withAsterisk                 :: Boolean
    , withinPortal                 :: Boolean
    , zIndex                       :: Maybe Number
    )

type MultiSelectItem =
  { value    :: String
  , label    :: Maybe String
  , disabled :: Maybe Boolean
  , group    :: Maybe String
  }

data MultiSelectDropdownPosition
  = MultiSelectDropdownPositionFlip
  | MultiSelectDropdownPositionBottom
  | MultiSelectDropdownPositionTop

instance DefaultValue MultiSelectDropdownPosition where defaultValue = MultiSelectDropdownPositionFlip

instance ToFFI MultiSelectDropdownPosition String where
  toNative = case _ of
    MultiSelectDropdownPositionFlip   -> "flip"
    MultiSelectDropdownPositionBottom -> "bottom"
    MultiSelectDropdownPositionTop    -> "top"

data MultiSelectClearable
  = MultiSelectNotClearable
  | MultiSelectClearable ClearButtonProps

instance DefaultValue MultiSelectClearable where defaultValue = MultiSelectNotClearable

data MultiSelectCreatable
  = MultiSelectNotCreatable
  | MultiSelectCreatable
      { getCreateLabel :: String -> String
      , onCreate       :: String -> Effect MultiSelectItem
      , shouldCreate   :: Maybe ({ query :: String, data :: Array MultiSelectItem } -> Boolean)
      }

instance DefaultValue MultiSelectCreatable where defaultValue = MultiSelectNotCreatable

type ClearablePropsImpl restImpl =
  ( clearable        :: Boolean
  , clearButtonProps :: Nullable ClearButtonPropsImpl
  | restImpl
  )

type CreatablePropsImpl restImpl =
  ( creatable      :: Boolean
  , getCreateLabel :: Nullable (String -> String)
  , onCreate       :: Nullable (EffectFn1 String MultiSelectItemImpl)
  , shouldCreate   :: Nullable ({ query :: String, data :: Nullable (Array MultiSelectItemImpl) } -> Boolean)
  | restImpl
  )

type MultiSelectPropsImpl =
  ThemingPropsImpl
    ( ClearablePropsImpl + CreatablePropsImpl
      ( clearSearchOnBlur            :: Boolean
      , clearSearchOnChange          :: Boolean
      , data                         :: Array MultiSelectItemImpl
      , defaultValue                 :: Nullable (Array String)
      , description                  :: Nullable JSX
      , disableSelectedItemFiltering :: Boolean
      , disabled                     :: Boolean
      , dropdownPosition             :: String
      , error                        :: Nullable JSX
      , filter                       :: Nullable (MultiSelectItemImpl -> Boolean)
      , hoverOnSearchChange          :: Boolean
      , icon                         :: Nullable JSX
      , iconWidth                    :: Nullable Number
      , initiallyOpened              :: Boolean
      , inputContainer               :: Nullable (JSX -> JSX)
      , inputWrapperOrder            :: Nullable (Array String)
      , itemComponent                :: Nullable (MultiSelectItemImpl -> JSX)
      , label                        :: Nullable JSX
      , limit                        :: Nullable Number
      , maxDropdownHeight            :: Nullable Number
      , maxSelectedValues            :: Nullable Number
      , nothingFound                 :: Nullable JSX
      , onChange                     :: Nullable (EffectFn1 (Nullable (Array String)) Unit)
      , onDropdownClose              :: Nullable (Effect Unit)
      , onDropdownOpen               :: Nullable (Effect Unit)
      , onSearchChange               :: Nullable (EffectFn1 String Unit)
      , radius                       :: Nullable MantineNumberSizeImpl
      , required                     :: Boolean
      , rightSection                 :: Nullable JSX
      , rightSectionWidth            :: Nullable Number
      , searchValue                  :: Nullable String
      , searchable                   :: Boolean
      , selectOnBlur                 :: Boolean
      , size                         :: Nullable String
      , switchDirectionOnFlip        :: Boolean
      , transitionProps              :: MantineTransitionPropsImpl
      , value                        :: Nullable (Array String)
      , valueComponent               :: Nullable (MultiSelectItemImpl -> JSX)
      , variant                      :: String
      , withAsterisk                 :: Boolean
      , withinPortal                 :: Boolean
      , zIndex                       :: Nullable Number
      )
    )

type MultiSelectItemImpl =
  { value    :: String
  , label    :: Nullable String
  , disabled :: Nullable Boolean
  , group    :: Nullable String
  }

multiSelectToImpl :: MultiSelectProps -> MultiSelectPropsImpl
multiSelectToImpl props =
  let otherProps =
        { itemComponent:  maybe null (\f -> notNull (f <<< fromNative)) props.itemComponent
        , inputContainer: toNullable props.inputContainer
        , valueComponent: maybe null (\f -> notNull (f <<< fromNative)) props.valueComponent
        , filter:         toNullable $ (\f -> f <<< fromNative) <$> props.filter
        , onChange:       toNullable $ (\h -> mkEffectFn1 (h <<< fromMaybe [] <<< toMaybe)) <$> props.onChange
        , onSearchChange: toNullable $ mkEffectFn1 <$> props.onSearchChange
        }

      rest = toNative
         <<< delete (Proxy :: Proxy "clearable")
         <<< delete (Proxy :: Proxy "creatable")
         <<< delete (Proxy :: Proxy "filter")
         <<< delete (Proxy :: Proxy "inputContainer")
         <<< delete (Proxy :: Proxy "itemComponent")
         <<< delete (Proxy :: Proxy "onChange")
         <<< delete (Proxy :: Proxy "onSearchChange")
         <<< delete (Proxy :: Proxy "valueComponent")

   in clearableProps props `union` creatableProps props `union` otherProps `union` rest props

clearableProps :: forall rest.
  Record ( clearable :: MultiSelectClearable | rest) -> Record (ClearablePropsImpl ())
clearableProps props = toNative $ case props.clearable of
  MultiSelectClearable p  -> { clearable: true, clearButtonProps: pure p }
  MultiSelectNotClearable -> { clearable: false, clearButtonProps: Nothing }

creatableProps :: forall rest.
  Record ( creatable :: MultiSelectCreatable | rest ) -> Record (CreatablePropsImpl ())
creatableProps props =
  let creatable = case props.creatable of
        MultiSelectCreatable _  -> true
        MultiSelectNotCreatable -> false

      getCreatable = case _ of
        MultiSelectCreatable c  -> pure c
        MultiSelectNotCreatable -> Nothing

      mkOnCreate onCreate = mkEffectFn1 \v -> toNative <$> onCreate v

      mkShouldCreate shouldCreate = \params -> shouldCreate { query: params.query, data: maybe [] (map fromNative) (toMaybe params.data) }

   in { creatable
      , getCreateLabel: toNullable $ _.getCreateLabel <$> getCreatable props.creatable
      , onCreate:       maybe null (notNull <<< mkOnCreate <<< _.onCreate) (getCreatable props.creatable)
      , shouldCreate:   toNullable $ mkShouldCreate <$> (_.shouldCreate =<< getCreatable props.creatable)
      }
