module Mantine.Core.Inputs.Select
  ( select
  , SelectProps
  , SelectItem
  , SelectClearable(..)
  , SelectCreatable(..)
  , SelectDropdownPosition(..)

  , module Mantine.Core.Inputs.Input
  ) where

import Prelude ((<$>))
import Data.Maybe (maybe)
import Effect.Uncurried (mkEffectFn1)
import Mantine.Core.Prelude
import Mantine.Core.Inputs.Input (InputVariant(..))

select :: (SelectProps -> SelectProps) -> JSX
select = mkComponent selectComponent selectToImpl defaultSelectProps

foreign import selectComponent :: ReactComponent SelectPropsImpl

type SelectProps =
  ThemingProps
    ( clearable                    :: SelectClearable
    , creatable                    :: SelectCreatable
    , data                         :: Array SelectItem
    , defaultValue                 :: Maybe String
    , description                  :: Maybe JSX
    , disabled                     :: Boolean
    , dropdownPosition             :: SelectDropdownPosition
    , error                        :: Maybe JSX
    , filter                       :: Maybe (SelectItem -> Boolean)
    , filterDataOnExactSearchMatch :: Boolean
    , icon                         :: Maybe JSX
    , iconWidth                    :: Maybe Pixels
    , initiallyOpened              :: Boolean
    , itemComponent                :: Maybe (SelectItem -> JSX)
    , label                        :: Maybe JSX
    , limit                        :: Maybe Int
    , maxDropdownHeight            :: Maybe Pixels
    , nothingFound                 :: Maybe JSX
    , onChange                     :: ValueHandler String
    , onDropdownClose              :: Effect Unit
    , onDropdownOpen               :: Effect Unit
    , onSearchChange               :: ValueHandler String
    , placeholder                  :: Maybe String
    , radius                       :: Maybe MantineNumberSize
    , required                     :: Boolean
    , rightSection                 :: Maybe JSX
    , rightSectionWidth            :: Maybe Pixels
    , searchValue                  :: Maybe String
    , searchable                   :: Boolean
    , selectOnBlur                 :: Boolean
    , size                         :: Maybe MantineSize
    , switchDirectionOnFlip        :: Boolean
    , transition                   :: Maybe MantineTransition
    , transitionDuration           :: Maybe Milliseconds
    , value                        :: Maybe String
    , variant                      :: InputVariant
    , withAsterisk                 :: Boolean
    , withinPortal                 :: Boolean
    , zIndex                       :: Maybe Number
    )

type SelectItem =
  { value    :: String
  , label    :: Maybe String
  , disabled :: Maybe Boolean
  , group    :: Maybe String
  }

data SelectDropdownPosition
  = SelectDropdownPositionFlip
  | SelectDropdownPositionBottom
  | SelectDropdownPositionTop

instance DefaultValue SelectDropdownPosition where defaultValue = SelectDropdownPositionFlip

instance ToFFI SelectDropdownPosition String where
  toNative = case _ of
    SelectDropdownPositionFlip   -> "flip"
    SelectDropdownPositionBottom -> "bottom"
    SelectDropdownPositionTop    -> "top"

data SelectClearable
  = SelectNotClearable
  | SelectClearable String

instance DefaultValue SelectClearable where defaultValue = SelectNotClearable

data SelectCreatable
  = SelectNotCreatable
  | SelectCreatable
      { getCreateLabel :: String -> JSX
      , onCreate       :: String -> Effect SelectItem
      , shouldCreate   :: { query :: String, data :: Array SelectItem } -> Boolean
      }

instance DefaultValue SelectCreatable where defaultValue = SelectNotCreatable

defaultSelectProps :: SelectProps
defaultSelectProps =
  defaultThemingProps
    { onDropdownClose: pure unit
    , onDropdownOpen:  pure unit
    }

type SelectPropsImpl =
  ThemingPropsImpl
    ( clearButtonLabel             :: Nullable String
    , clearable                    :: Boolean
    , creatable                    :: Boolean
    , data                         :: Array SelectItemImpl
    , defaultValue                 :: Nullable String
    , description                  :: Nullable JSX
    , disabled                     :: Boolean
    , dropdownPosition             :: String
    , error                        :: Nullable JSX
    , filter                       :: Nullable (SelectItemImpl -> Boolean)
    , filterDataOnExactSearchMatch :: Boolean
    , getCreateLabel               :: Nullable (String -> JSX)
    , icon                         :: Nullable JSX
    , iconWidth                    :: Nullable Number
    , initiallyOpened              :: Boolean
    , itemComponent                :: Nullable (SelectItemImpl -> JSX)
    , label                        :: Nullable JSX
    , limit                        :: Nullable Number
    , maxDropdownHeight            :: Nullable Number
    , nothingFound                 :: Nullable JSX
    , onChange                     :: EffectFn1 String Unit
    , onCreate                     :: Nullable (EffectFn1 String SelectItemImpl)
    , onDropdownClose              :: Effect Unit
    , onDropdownOpen               :: Effect Unit
    , onSearchChange               :: EffectFn1 String Unit
    , placeholder                  :: Nullable String
    , radius                       :: Nullable MantineNumberSizeImpl
    , required                     :: Boolean
    , rightSection                 :: Nullable JSX
    , rightSectionWidth            :: Nullable Number
    , searchValue                  :: Nullable String
    , searchable                   :: Boolean
    , selectOnBlur                 :: Boolean
    , shouldCreate                 :: Nullable ({ query :: String, data :: Array SelectItemImpl } -> Boolean)
    , size                         :: Nullable String
    , switchDirectionOnFlip        :: Boolean
    , transition                   :: Nullable String
    , transitionDuration           :: Nullable Number
    , value                        :: Nullable String
    , variant                      :: String
    , withAsterisk                 :: Boolean
    , withinPortal                 :: Boolean
    , zIndex                       :: Nullable Number
    )

type SelectItemImpl =
  { value    :: String
  , label    :: Nullable String
  , disabled :: Nullable Boolean
  , group    :: Nullable String
  }

selectToImpl :: SelectProps -> SelectPropsImpl
selectToImpl props =
  let isClearable = case _ of
        SelectClearable _  -> true
        SelectNotClearable -> false
      getClearButtonLabel = case _ of
        SelectClearable l  -> pure l
        SelectNotClearable -> Nothing
      isCreatable = case _ of
        SelectCreatable _  -> true
        SelectNotCreatable -> false
      getCreatable = case _ of
        SelectCreatable c  -> pure c
        SelectNotCreatable -> Nothing
      mkOnCreate onCreate = mkEffectFn1 \v -> toNative <$> onCreate v

      mkShouldCreate shouldCreate = \params -> shouldCreate { query: params.query, data: fromNative <$> params.data }

      rest = toNative
         <<< delete (Proxy :: Proxy "clearable")
         <<< delete (Proxy :: Proxy "creatable")
         <<< delete (Proxy :: Proxy "filter")
         <<< delete (Proxy :: Proxy "itemComponent")

   in { clearable:        isClearable props.clearable
      , clearButtonLabel: toNullable $ getClearButtonLabel props.clearable

      , creatable:        isCreatable props.creatable
      , getCreateLabel:   toNullable $ _.getCreateLabel <$> getCreatable props.creatable
      , onCreate:         maybe null (notNull <<< mkOnCreate <<< _.onCreate) (getCreatable props.creatable)
      , shouldCreate:     toNullable $ (mkShouldCreate <<< _.shouldCreate) <$> getCreatable props.creatable

      , filter:           toNullable $ (\f -> f <<< fromNative) <$> props.filter
      , itemComponent:    maybe null (\f -> notNull (f <<< fromNative)) props.itemComponent
      } `union` rest props
