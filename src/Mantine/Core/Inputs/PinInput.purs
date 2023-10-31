module Mantine.Core.Inputs.PinInput
  ( pinInput
  , PinInputProps
  , PinInputMode(..)
  , PinInputType(..)

  , module Mantine.Core.Inputs.Input
  ) where

import Data.String.Regex (Regex)
import Mantine.Core.Inputs.Input (InputType(..))
import Mantine.Core.Prelude

pinInput :: (PinInputProps -> PinInputProps) -> JSX
pinInput = mkTrivialComponent pinInputComponent

foreign import pinInputComponent :: ReactComponent PinInputPropsImpl

-- Not supported properties:
--   { hiddenInputProps :: Omit<DetailedHTMLProps<InputHTMLAttributes<HTMLInputElement>, HTMLInputElement>, "ref">
--   }

type PinInputProps =
  ThemingProps
    ( ariaLabel    :: Maybe String
    , autoFocus    :: Boolean
    , defaultValue :: Maybe String
    , disabled     :: Boolean
    , error        :: Boolean
    , form         :: Maybe String
    , gap          :: Maybe MantineSpacing
    , id           :: Maybe String
    , inputMode    :: Maybe PinInputMode
    , inputType    :: Maybe InputType
    , length       :: Number
    , manageFocus  :: Boolean
    , mask         :: Boolean
    , name         :: Maybe String
    , onChange     :: ValueHandler String
    , onComplete   :: ValueHandler String
    , oneTimeCode  :: Boolean
    , placeholder  :: Maybe String
    , radius       :: Maybe MantineNumberSize
    , readOnly     :: Boolean
    , size         :: Maybe MantineSize
    , type         :: PinInputType
    , value        :: Maybe String
    )

data PinInputMode
  = PinInputModeText
  | PinInputModeNone
  | PinInputModeSearch
  | PinInputModeTel
  | PinInputModeUrl
  | PinInputModeEmail
  | PinInputModeNumeric
  | PinInputModeDecimal

instance ToFFI PinInputMode String where
  toNative = case _ of
    PinInputModeText    -> "text"
    PinInputModeNone    -> "none"
    PinInputModeSearch  -> "search"
    PinInputModeTel     -> "tel"
    PinInputModeUrl     -> "url"
    PinInputModeEmail   -> "email"
    PinInputModeNumeric -> "numeric"
    PinInputModeDecimal -> "decimal"

data PinInputType
  = PinInputTypeAlphanumeric
  | PinInputTypeNumber
  | PinInputTypeRegExp Regex

type PinInputTypeImpl = String |+| Regex

instance DefaultValue PinInputType where defaultValue = PinInputTypeAlphanumeric

instance ToFFI PinInputType PinInputTypeImpl where
  toNative = case _ of
    PinInputTypeAlphanumeric -> asOneOf "alphanumeric"
    PinInputTypeNumber       -> asOneOf "number"
    PinInputTypeRegExp r     -> asOneOf r

type PinInputPropsImpl =
  ThemingPropsImpl
    ( ariaLabel    :: Nullable String
    , autoFocus    :: Boolean
    , defaultValue :: Nullable String
    , disabled     :: Boolean
    , error        :: Boolean
    , form         :: Nullable String
    , gap          :: Nullable MantineSpacingImpl
    , id           :: Nullable String
    , inputMode    :: Nullable String
    , inputType    :: Nullable String
    , length       :: Number
    , manageFocus  :: Boolean
    , mask         :: Boolean
    , name         :: Nullable String
    , onChange     :: EffectFn1 String Unit
    , onComplete   :: EffectFn1 String Unit
    , oneTimeCode  :: Boolean
    , placeholder  :: Nullable String
    , radius       :: Nullable MantineNumberSizeImpl
    , readOnly     :: Boolean
    , size         :: Nullable String
    , type         :: PinInputTypeImpl
    , value        :: Nullable String
    )
