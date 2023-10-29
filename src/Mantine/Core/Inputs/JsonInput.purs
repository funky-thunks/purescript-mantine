module Mantine.Core.Inputs.JsonInput
  ( jsonInput
  , JsonInputProps

  , module Mantine.Core.Inputs.Input
  ) where

import Mantine.Core.Inputs.Input (InputVariant(..), InputWrapperOrder(..))
import Mantine.Core.Prelude

jsonInput :: (JsonInputProps -> JsonInputProps) -> JSX
jsonInput = mkComponent jsonInputComponent jsonInputToImpl defaultThemingProps_

foreign import jsonInputComponent :: ReactComponent JsonInputPropsImpl

-- Not supported properties:
--   { deserialize :: (text: string, reviver?: (this: any, key: string, value: any) => any) => any
--   , serialize   :: { (value: any, replacer?: (this: any, key: string, value: any) => any, space?: string | number): string; (value: any, replacer?: (string | number)[], space?: string | number): string; }
--   }

type JsonInputProps =
  ThemingProps
    ( autosize          :: Boolean
    , defaultValue      :: Maybe String
    , description       :: Maybe JSX
    , disabled          :: Boolean
    , error             :: Maybe JSX
    , formatOnBlur      :: Boolean
    , icon              :: Maybe JSX
    , iconWidth         :: Maybe Pixels
    , id                :: Maybe String
    , inputContainer    :: Maybe (JSX -> JSX)
    , inputWrapperOrder :: Maybe (Array InputWrapperOrder)
    , label             :: Maybe JSX
    , maxRows           :: Maybe Int
    , minRows           :: Maybe Int
    , onChange          :: InputHandler
    , placeholder       :: Maybe String
    , radius            :: Maybe MantineNumberSize
    , required          :: Boolean
    , rightSection      :: Maybe JSX
    , rightSectionWidth :: Maybe Pixels
    , size              :: Maybe MantineNumberSize
    , validationError   :: Maybe JSX
    , value             :: Maybe String
    , variant           :: InputVariant
    , withAsterisk      :: Boolean
    )

type JsonInputPropsImpl =
  ThemingPropsImpl
    ( autosize          :: Boolean
    , defaultValue      :: Nullable String
    , description       :: Nullable JSX
    , disabled          :: Boolean
    , error             :: Nullable JSX
    , formatOnBlur      :: Boolean
    , icon              :: Nullable JSX
    , iconWidth         :: Nullable Number
    , id                :: Nullable String
    , inputContainer    :: Nullable (JSX -> JSX)
    , inputWrapperOrder :: Nullable (Array String)
    , label             :: Nullable JSX
    , maxRows           :: Nullable Number
    , minRows           :: Nullable Number
    , onChange          :: EffectFn1 SyntheticEvent Unit
    , placeholder       :: Nullable String
    , radius            :: Nullable MantineNumberSizeImpl
    , required          :: Boolean
    , rightSection      :: Nullable JSX
    , rightSectionWidth :: Nullable Number
    , size              :: Nullable MantineNumberSizeImpl
    , validationError   :: Nullable JSX
    , value             :: Nullable String
    , variant           :: String
    , withAsterisk      :: Boolean
    )

jsonInputToImpl :: JsonInputProps -> JsonInputPropsImpl
jsonInputToImpl props =
  let rest = toNative <<< delete (Proxy :: Proxy "inputContainer")
      inputContainer = toNullable props.inputContainer
   in { inputContainer } `union` rest props
