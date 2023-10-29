module Mantine.Core.Inputs.Select
  ( select
  , SelectProps
  , SelectItem
  , SelectClearable(..)
  , SelectCreatable(..)
  , SelectDropdownPosition(..)

  , module Mantine.Core.Inputs.ClearButtonProps
  , module Mantine.Core.Inputs.Input

  , SelectItemImpl
  ) where

import Data.Maybe (maybe)
import Effect.Uncurried (mkEffectFn1)
import Mantine.Core.Inputs.ClearButtonProps (ClearButtonProps, ClearButtonPropsImpl)
import Mantine.Core.Inputs.Input (InputVariant(..), InputWrapperOrder(..))
import Mantine.Core.Prelude

select :: (SelectProps -> SelectProps) -> JSX
select = mkComponent selectComponent selectToImpl defaultSelectProps

foreign import selectComponent :: ReactComponent SelectPropsImpl

type SelectProps =
  ThemingProps
    ( allowDeselect                :: Boolean
    , clearable                    :: SelectClearable
    , creatable                    :: SelectCreatable
    , data                         :: Array SelectItem
    , defaultValue                 :: Maybe String
    , description                  :: Maybe JSX
    , disabled                     :: Boolean
    , dropdownPosition             :: SelectDropdownPosition
    , error                        :: Maybe JSX
    , filter                       :: Maybe (SelectItem -> Boolean)
    , filterDataOnExactSearchMatch :: Boolean
    , hoverOnSearchChange          :: Boolean
    , icon                         :: Maybe JSX
    , iconWidth                    :: Maybe Pixels
    , initiallyOpened              :: Boolean
    , inputContainer               :: Maybe (JSX -> JSX)
    , inputWrapperOrder            :: Maybe (Array InputWrapperOrder)
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
    , transitionProps              :: MantineTransitionProps
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
  | SelectClearable ClearButtonProps

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

type ClearablePropsImpl restImpl =
  ( clearable        :: Boolean
  , clearButtonProps :: Nullable ClearButtonPropsImpl
  | restImpl
  )

type CreatablePropsImpl restImpl =
  ( creatable      :: Boolean
  , getCreateLabel :: Nullable (String -> JSX)
  , onCreate       :: Nullable (EffectFn1 String SelectItemImpl)
  , shouldCreate   :: Nullable ({ query :: String, data :: Array SelectItemImpl } -> Boolean)
  | restImpl
  )

type SelectPropsImpl =
  ThemingPropsImpl
    ( ClearablePropsImpl + CreatablePropsImpl
      ( allowDeselect                :: Boolean
      , data                         :: Array SelectItemImpl
      , defaultValue                 :: Nullable String
      , description                  :: Nullable JSX
      , disabled                     :: Boolean
      , dropdownPosition             :: String
      , error                        :: Nullable JSX
      , filter                       :: Nullable (SelectItemImpl -> Boolean)
      , filterDataOnExactSearchMatch :: Boolean
      , hoverOnSearchChange          :: Boolean
      , icon                         :: Nullable JSX
      , iconWidth                    :: Nullable Number
      , initiallyOpened              :: Boolean
      , inputContainer               :: Nullable (JSX -> JSX)
      , inputWrapperOrder            :: Nullable (Array String)
      , itemComponent                :: Nullable (SelectItemImpl -> JSX)
      , label                        :: Nullable JSX
      , limit                        :: Nullable Number
      , maxDropdownHeight            :: Nullable Number
      , nothingFound                 :: Nullable JSX
      , onChange                     :: EffectFn1 String Unit
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
      , size                         :: Nullable String
      , switchDirectionOnFlip        :: Boolean
      , transitionProps              :: MantineTransitionPropsImpl
      , value                        :: Nullable String
      , variant                      :: String
      , withAsterisk                 :: Boolean
      , withinPortal                 :: Boolean
      , zIndex                       :: Nullable Number
      )
    )

type SelectItemImpl =
  { value    :: String
  , label    :: Nullable String
  , disabled :: Nullable Boolean
  , group    :: Nullable String
  }

selectToImpl :: SelectProps -> SelectPropsImpl
selectToImpl props =
  let otherProps =
        { filter:        toNullable $ (\f -> f <<< fromNative) <$> props.filter
        , itemComponent: maybe null (\f -> notNull (f <<< fromNative)) props.itemComponent
        , inputContainer: toNullable props.inputContainer
        }

      rest = toNative
         <<< delete (Proxy :: Proxy "clearable")
         <<< delete (Proxy :: Proxy "creatable")
         <<< delete (Proxy :: Proxy "filter")
         <<< delete (Proxy :: Proxy "itemComponent")
         <<< delete (Proxy :: Proxy "inputContainer")

   in clearableProps props `union` creatableProps props `union` otherProps `union` rest props

clearableProps :: forall rest.
  Record ( clearable :: SelectClearable | rest) -> Record (ClearablePropsImpl ())
clearableProps props = toNative $ case props.clearable of
  SelectClearable p  -> { clearable: true,  clearButtonProps: Just p  }
  SelectNotClearable -> { clearable: false, clearButtonProps: Nothing }

creatableProps :: forall rest.
  Record ( creatable :: SelectCreatable | rest ) -> Record (CreatablePropsImpl ())
creatableProps props =
  let creatable = case props.creatable of
        SelectCreatable _  -> true
        SelectNotCreatable -> false

      getCreatable = case _ of
        SelectCreatable c  -> pure c
        SelectNotCreatable -> Nothing

      mkOnCreate onCreate = mkEffectFn1 \v -> toNative <$> onCreate v

      mkShouldCreate shouldCreate = \params -> shouldCreate { query: params.query, data: fromNative <$> params.data }

   in { creatable
      , getCreateLabel: toNullable $ _.getCreateLabel <$> getCreatable props.creatable
      , onCreate:       maybe null (notNull <<< mkOnCreate <<< _.onCreate) (getCreatable props.creatable)
      , shouldCreate:   toNullable $ (mkShouldCreate <<< _.shouldCreate) <$> getCreatable props.creatable
      }
