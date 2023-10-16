module Mantine.Core.Inputs.Checkbox
  ( checkbox
  , CheckboxProps
  , CheckboxLabelPosition(..)

  , checkboxGroup
  , checkboxGroup_
  , CheckboxGroupProps

  , module Mantine.Core.Common
  ) where

import Prelude hiding (bind)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import Mantine.Core.Common (AlignItems(..), MantineColor(..), MantineNumberSize, MantineSize(..), Orientation(..), Radius(..))
import Mantine.Core.Common as MC
import Mantine.FFI (class ToFFI, toNative)
import React.Basic (ReactComponent, element)
import React.Basic.Events (SyntheticEvent)
import React.Basic.Hooks (JSX)
import Record (delete, merge)
import Type.Proxy (Proxy(..))

checkbox :: (CheckboxProps -> CheckboxProps) -> JSX
checkbox setProps = element checkboxComponent (toNativeCheckbox (setProps MC.defaultThemingProps_))

foreign import checkboxComponent :: ReactComponent CheckboxPropsImpl

type CheckboxProps =
  MC.ThemingProps
    ( checked            :: Maybe Boolean
    , color              :: Maybe MantineColor
    , description        :: Maybe JSX
    , disabled           :: Boolean
    , error              :: Maybe JSX
    , icon               :: Maybe ({ indeterminate :: Boolean, className :: String } -> JSX)
    , id                 :: Maybe String
    , indeterminate      :: Maybe Boolean
    , label              :: Maybe JSX
    , labelPosition      :: Maybe CheckboxLabelPosition
    , onChange           :: Maybe (SyntheticEvent -> Effect Unit)
    , radius             :: Maybe MantineNumberSize
    , size               :: Maybe MantineSize
    , transitionDuration :: Maybe Number
    , value              :: Maybe String
    )

type CheckboxPropsImpl =
  MC.ThemingPropsImpl
    ( checked            :: Nullable Boolean
    , color              :: Nullable String
    , description        :: Nullable JSX
    , disabled           :: Boolean
    , error              :: Nullable JSX
    , icon               :: Nullable ({ indeterminate :: Boolean, className :: String } -> JSX)
    , id                 :: Nullable String
    , indeterminate      :: Nullable Boolean
    , label              :: Nullable JSX
    , labelPosition      :: Nullable String
    , onChange           :: Nullable (EffectFn1 SyntheticEvent Unit)
    , radius             :: Nullable MC.MantineNumberSizeImpl
    , size               :: Nullable String
    , transitionDuration :: Nullable Number
    , value              :: Nullable String
    )

data CheckboxLabelPosition
  = CheckboxLabelPositionLeft
  | CheckboxLabelPositionRight

instance ToFFI CheckboxLabelPosition String where
  toNative = case _ of
    CheckboxLabelPositionLeft  -> "left"
    CheckboxLabelPositionRight -> "right"

toNativeCheckbox :: CheckboxProps -> CheckboxPropsImpl
toNativeCheckbox props = toNative (delete (Proxy :: Proxy "icon") props) `merge` { icon: toNullable props.icon }

checkboxGroup :: (CheckboxGroupProps -> CheckboxGroupProps) -> JSX
checkboxGroup setProps = element checkboxGroupComponent (toNative (setProps MC.defaultThemingProps_))

checkboxGroup_ :: Array JSX -> JSX
checkboxGroup_ children = checkboxGroup _ { children = children }

foreign import checkboxGroupComponent :: ReactComponent CheckboxGroupPropsImpl

type CheckboxGroupProps =
  MC.ThemingProps
    ( children     :: Array JSX
    , defaultValue :: Maybe (Array String)
    , description  :: Maybe JSX
    , error        :: Maybe JSX
    , label        :: Maybe JSX
    , offset       :: Maybe MantineNumberSize
    , onChange     :: Maybe (Array String -> Effect Unit)
    , orientation  :: Maybe Orientation
    , required     :: Maybe Boolean
    , size         :: Maybe MantineSize
    , value        :: Maybe (Array String)
    , withAsterisk :: Maybe Boolean
    )

type CheckboxGroupPropsImpl =
  MC.ThemingPropsImpl
    ( children     :: Array JSX
    , defaultValue :: Nullable (Array String)
    , description  :: Nullable JSX
    , error        :: Nullable JSX
    , label        :: Nullable JSX
    , offset       :: Nullable MC.MantineNumberSizeImpl
    , onChange     :: Nullable (EffectFn1 (Array String) Unit)
    , orientation  :: Nullable String
    , required     :: Nullable Boolean
    , size         :: Nullable String
    , value        :: Nullable (Array String)
    , withAsterisk :: Nullable Boolean
    )
