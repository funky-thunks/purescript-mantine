module Mantine.Core.Inputs.JsonInput
  ( jsonInput
  , JsonInputProps

  , module Mantine.Core.Inputs.Input
  ) where

import Mantine.Core.Prelude
import Mantine.Core.Inputs.Input (InputVariant(..))

jsonInput :: (JsonInputProps -> JsonInputProps) -> JSX
jsonInput = mkTrivialComponent jsonInputComponent

foreign import jsonInputComponent :: ReactComponent JsonInputPropsImpl

type JsonInputProps =
  ThemingProps
    ( autosize        :: Boolean
    , defaultValue    :: Maybe String
    , description     :: Maybe JSX
    , disabled        :: Boolean
    , error           :: Maybe JSX
    , icon            :: Maybe JSX
    , iconWidth       :: Maybe Pixels
    , id              :: Maybe String
    , label           :: Maybe JSX
    , maxRows         :: Maybe Int
    , minRows         :: Maybe Int
    , onChange        :: InputHandler
    , placeholder     :: Maybe String
    , radius          :: Maybe MantineNumberSize
    , required        :: Boolean
    , rightSection    :: Maybe JSX
    , size            :: Maybe MantineNumberSize
    , validationError :: Maybe JSX
    , value           :: Maybe String
    , variant         :: InputVariant
    , withAsterisk    :: Boolean
    )

type JsonInputPropsImpl =
  ThemingPropsImpl
    ( autosize        :: Boolean
    , defaultValue    :: Nullable String
    , description     :: Nullable JSX
    , disabled        :: Boolean
    , error           :: Nullable JSX
    , icon            :: Nullable JSX
    , iconWidth       :: Nullable Number
    , id              :: Nullable String
    , label           :: Nullable JSX
    , maxRows         :: Nullable Number
    , minRows         :: Nullable Number
    , onChange        :: EffectFn1 SyntheticEvent Unit
    , placeholder     :: Nullable String
    , radius          :: Nullable MantineNumberSizeImpl
    , required        :: Boolean
    , rightSection    :: Nullable JSX
    , size            :: Nullable MantineNumberSizeImpl
    , validationError :: Nullable JSX
    , value           :: Nullable String
    , variant         :: String
    , withAsterisk    :: Boolean
    )
