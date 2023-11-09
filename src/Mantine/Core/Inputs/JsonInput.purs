module Mantine.Core.Inputs.JsonInput
  ( jsonInput
  , JsonInputProps
  ) where

import Mantine.Core.Inputs.Input (InputComponent, InputComponentImpl)
import Mantine.Core.Prelude

jsonInput :: (JsonInputProps -> JsonInputProps) -> JSX
jsonInput = mkTrivialComponent jsonInputComponent

foreign import jsonInputComponent :: ReactComponent JsonInputPropsImpl

-- Not supported properties:
--   { deserialize :: (text: string, reviver?: (this: any, key: string, value: any) => any) => any
--   , serialize   :: { (value: any, replacer?: (this: any, key: string, value: any) => any, space?: string | number): string; (value: any, replacer?: (string | number)[], space?: string | number): string; }
--   }

type JsonInputProps =
  InputComponent
    ( autosize        :: Boolean
    , defaultValue    :: Maybe String
    , formatOnBlur    :: Boolean
    , maxRows         :: Maybe Int
    , minRows         :: Maybe Int
    , onChange        :: InputHandler
    , validationError :: Maybe JSX
    , value           :: Maybe String
    )

type JsonInputPropsImpl =
  InputComponentImpl
    ( autosize        :: Boolean
    , defaultValue    :: Nullable String
    , formatOnBlur    :: Boolean
    , maxRows         :: Nullable Number
    , minRows         :: Nullable Number
    , onChange        :: InputHandlerImpl
    , validationError :: Nullable JSX
    , value           :: Nullable String
    )
