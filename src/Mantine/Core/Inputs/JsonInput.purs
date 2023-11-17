module Mantine.Core.Inputs.JsonInput
  ( jsonInput
  , Props_JsonInput
  , Props_JsonInputImpl
  ) where

import Mantine.Core.Inputs.Input (Props_InputComponent, Props_InputComponentImpl)
import Mantine.Core.Prelude

jsonInput
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_JsonInput
  => Union attrsImpl attrsImpl_ Props_JsonInputImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
jsonInput = element (unsafeCoerce jsonInputComponent) <<< toNative

foreign import jsonInputComponent :: ReactComponent (Record Props_JsonInputImpl)

-- Not supported properties:
--   { deserialize :: (text: string, reviver?: (this: any, key: string, value: any) => any) => any
--   , serialize   :: { (value: any, replacer?: (this: any, key: string, value: any) => any, space?: string | number): string; (value: any, replacer?: (string | number)[], space?: string | number): string; }
--   }

type Props_JsonInput =
  Props_InputComponent
    ( autosize        :: Boolean
    , defaultValue    :: String
    , formatOnBlur    :: Boolean
    , maxRows         :: Int
    , minRows         :: Int
    , onChange        :: InputHandler
    , validationError :: JSX
    , value           :: String
    )

type Props_JsonInputImpl =
  Props_InputComponentImpl
    ( autosize        :: Boolean
    , defaultValue    :: String
    , formatOnBlur    :: Boolean
    , maxRows         :: Number
    , minRows         :: Number
    , onChange        :: InputHandlerImpl
    , validationError :: JSX
    , value           :: String
    )
