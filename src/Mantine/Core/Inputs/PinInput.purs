module Mantine.Core.Inputs.PinInput
  ( pinInput
  , PinInputProps
  , PinInputMode(..)
  , PinInputType(..)

  , module Mantine.Core.Inputs.Input
  ) where

import Data.String.Regex (Regex)
import Mantine.Core.Inputs.Input (InputType(..), InputTypeImpl)
import Mantine.Core.Prelude

pinInput :: (PinInputProps -> PinInputProps) -> JSX
pinInput = mkTrivialComponent pinInputComponent

foreign import pinInputComponent :: ReactComponent PinInputPropsImpl

-- Not supported properties:
--   { hiddenInputProps :: Omit<DetailedHTMLProps<InputHTMLAttributes<HTMLInputElement>, HTMLInputElement>, "ref">
--   }

type PinInputProps =
  MantineComponent
    ( ariaLabel   :: Optional String
    , autoFocus   :: Boolean
    , disabled    :: Boolean
    , error       :: Boolean
    , form        :: Optional String
    , gap         :: Optional MantineSpacing
    , id          :: Optional String
    , inputMode   :: Optional PinInputMode
    , inputType   :: Optional InputType
    , length      :: Number
    , manageFocus :: Boolean
    , mask        :: Boolean
    , name        :: Optional String
    , onComplete  :: ValueHandler String
    , oneTimeCode :: Boolean
    , placeholder :: Optional String
    , radius      :: Optional MantineNumberSize
    , readOnly    :: Boolean
    , size        :: Optional MantineSize
    , type        :: PinInputType
    | Controlled String
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

type PinInputModeImpl = String

instance ToFFI PinInputMode PinInputModeImpl where
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
  MantineComponentImpl
    ( ariaLabel   :: OptionalImpl String
    , autoFocus   :: Boolean
    , disabled    :: Boolean
    , error       :: Boolean
    , form        :: OptionalImpl String
    , gap         :: OptionalImpl MantineSpacingImpl
    , id          :: OptionalImpl String
    , inputMode   :: OptionalImpl PinInputModeImpl
    , inputType   :: OptionalImpl InputTypeImpl
    , length      :: Number
    , manageFocus :: Boolean
    , mask        :: Boolean
    , name        :: OptionalImpl String
    , onComplete  :: ValueHandlerImpl String
    , oneTimeCode :: Boolean
    , placeholder :: OptionalImpl String
    , radius      :: OptionalImpl MantineNumberSizeImpl
    , readOnly    :: Boolean
    , size        :: OptionalImpl MantineSizeImpl
    , type        :: PinInputTypeImpl
    | ControlledImpl String
    )
