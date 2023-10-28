module Mantine.Core.Inputs.NativeSelect
  ( nativeSelect
  , NativeSelectProps

  , module Mantine.Core.Inputs.Input
  ) where

import Mantine.Core.Inputs.Input (InputVariant(..), InputWrapperOrder(..))
import Mantine.Core.Inputs.Select (SelectItem, SelectItemImpl)
import Mantine.Core.Prelude

nativeSelect :: (NativeSelectProps -> NativeSelectProps) -> JSX
nativeSelect = mkComponent nativeSelectComponent nativeSelectToImpl defaultThemingProps_

foreign import nativeSelectComponent :: ReactComponent NativeSelectPropsImpl

-- Not supported properties
--   { descriptionProps  :: Record<String, any>
--   , errorProps        :: Record<String, any>
--   , labelProps        :: Record<String, any>
--   , rightSectionProps :: Record<String, any>
--   , wrapperProps      :: Record<String, any>
--   }

type NativeSelectProps =
  ThemingProps
    ( data              :: Array SelectItem
    , description       :: Maybe JSX
    , disabled          :: Boolean
    , error             :: Maybe JSX
    , icon              :: Maybe JSX
    , iconWidth         :: Maybe Pixels
    , id                :: Maybe String
    , inputContainer    :: Maybe (JSX -> JSX)
    , inputWrapperOrder :: Maybe (Array InputWrapperOrder)
    , label             :: Maybe JSX
    , onChange          :: InputHandler
    , placeholder       :: Maybe String
    , radius            :: Maybe MantineNumberSize
    , required          :: Boolean
    , rightSection      :: Maybe JSX
    , rightSectionWidth :: Maybe Pixels
    , size              :: Maybe MantineSize
    , value             :: Maybe String
    , variant           :: InputVariant
    , withAsterisk      :: Boolean
    )

type NativeSelectPropsImpl =
  ThemingPropsImpl
    ( data              :: Array SelectItemImpl
    , description       :: Nullable JSX
    , disabled          :: Boolean
    , error             :: Nullable JSX
    , icon              :: Nullable JSX
    , iconWidth         :: Nullable Number
    , id                :: Nullable String
    , inputContainer    :: Nullable (JSX -> JSX)
    , inputWrapperOrder :: Nullable (Array String)
    , label             :: Nullable JSX
    , onChange          :: EffectFn1 SyntheticEvent Unit
    , placeholder       :: Nullable String
    , radius            :: Nullable MantineNumberSizeImpl
    , required          :: Boolean
    , rightSection      :: Nullable JSX
    , rightSectionWidth :: Nullable Number
    , size              :: Nullable String
    , value             :: Nullable String
    , variant           :: String
    , withAsterisk      :: Boolean
    )

nativeSelectToImpl :: NativeSelectProps -> NativeSelectPropsImpl
nativeSelectToImpl props =
  let rest = toNative
         <<< delete (Proxy :: Proxy "inputContainer")
   in { inputContainer: toNullable props.inputContainer } `union` rest props
