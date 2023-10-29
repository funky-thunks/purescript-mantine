module Mantine.Core.Inputs.Checkbox
  ( checkbox
  , CheckboxProps
  , CheckboxLabelPosition(..)

  , checkboxGroup
  , checkboxGroup_
  , CheckboxGroupProps

  , module Mantine.Core.Inputs.Input
  ) where

import Mantine.Core.Inputs.Input (InputWrapperOrder(..))
import Mantine.Core.Prelude

checkbox :: (CheckboxProps -> CheckboxProps) -> JSX
checkbox = mkComponent checkboxComponent toNativeCheckbox defaultThemingProps_

foreign import checkboxComponent :: ReactComponent CheckboxPropsImpl

type CheckboxProps =
  ThemingProps
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
  ThemingPropsImpl
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
    , radius             :: Nullable MantineNumberSizeImpl
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
toNativeCheckbox props =
  let rest = toNative <<< delete (Proxy :: Proxy "icon")
   in { icon: toNullable props.icon } `union` rest props

checkboxGroup :: (CheckboxGroupProps -> CheckboxGroupProps) -> JSX
checkboxGroup = mkComponent checkboxGroupComponent checkboxGroupToImpl defaultThemingProps_

checkboxGroup_ :: Array JSX -> JSX
checkboxGroup_ children = checkboxGroup _ { children = children }

foreign import checkboxGroupComponent :: ReactComponent CheckboxGroupPropsImpl

type CheckboxGroupProps =
  ThemingProps
    ( children          :: Array JSX
    , defaultValue      :: Maybe (Array String)
    , description       :: Maybe JSX
    , error             :: Maybe JSX
    , inputContainer    :: Maybe (JSX -> JSX)
    , inputWrapperOrder :: Maybe (Array InputWrapperOrder)
    , label             :: Maybe JSX
    , onChange          :: Maybe (Array String -> Effect Unit)
    , required          :: Maybe Boolean
    , size              :: Maybe MantineSize
    , value             :: Maybe (Array String)
    , withAsterisk      :: Maybe Boolean
    )

type CheckboxGroupPropsImpl =
  ThemingPropsImpl
    ( children          :: Array JSX
    , defaultValue      :: Nullable (Array String)
    , description       :: Nullable JSX
    , error             :: Nullable JSX
    , inputContainer    :: Nullable (JSX -> JSX)
    , inputWrapperOrder :: Nullable (Array String)
    , label             :: Nullable JSX
    , onChange          :: Nullable (EffectFn1 (Array String) Unit)
    , required          :: Nullable Boolean
    , size              :: Nullable String
    , value             :: Nullable (Array String)
    , withAsterisk      :: Nullable Boolean
    )

checkboxGroupToImpl :: CheckboxGroupProps -> CheckboxGroupPropsImpl
checkboxGroupToImpl props =
  let rest = toNative <<< delete (Proxy :: Proxy "inputContainer")
      inputContainer = toNullable props.inputContainer
   in { inputContainer } `union` rest props
