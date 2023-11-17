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
    , defaultValue    :: Optional String
    , formatOnBlur    :: Boolean
    , maxRows         :: Optional Int
    , minRows         :: Optional Int
    , onChange        :: InputHandler
    , validationError :: Optional JSX
    , value           :: Optional String
    )

type JsonInputPropsImpl =
  InputComponentImpl
    ( autosize        :: Boolean
    , defaultValue    :: OptionalImpl String
    , formatOnBlur    :: Boolean
    , maxRows         :: OptionalImpl Number
    , minRows         :: OptionalImpl Number
    , onChange        :: InputHandlerImpl
    , validationError :: OptionalImpl JSX
    , value           :: OptionalImpl String
    )
