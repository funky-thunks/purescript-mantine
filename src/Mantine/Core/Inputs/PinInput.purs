module Mantine.Core.Inputs.PinInput
  ( pinInput
  , Props_PinInput
  , Props_PinInputImpl
  , PinInputMode(..)
  , PinInputModeImpl
  , PinInputType(..)
  , PinInputTypeImpl

  , module Mantine.Core.Inputs.Input
  ) where

import Data.String.Regex (Regex)
import Mantine.Core.Inputs.Input (InputType(..), InputTypeImpl)
import Mantine.Core.Prelude

pinInput
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_PinInput
  => Union attrsImpl attrsImpl_ Props_PinInputImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
pinInput = element (unsafeCoerce pinInputComponent) <<< toNative

foreign import pinInputComponent :: ReactComponent (Record Props_PinInputImpl)

-- Not supported properties:
--   { hiddenInputProps :: Omit<DetailedHTMLProps<InputHTMLAttributes<HTMLInputElement>, HTMLInputElement>, "ref">
--   }

type Props_PinInput =
  Props_Common
    ( ariaLabel   :: String
    , autoFocus   :: Boolean
    , disabled    :: Boolean
    , error       :: Boolean
    , form        :: String
    , gap         :: MantineSpacing
    , id          :: String
    , inputMode   :: PinInputMode
    , inputType   :: InputType
    , length      :: Number
    , manageFocus :: Boolean
    , mask        :: Boolean
    , name        :: String
    , onComplete  :: ValueHandler String
    , oneTimeCode :: Boolean
    , placeholder :: String
    , radius      :: MantineNumberSize
    , readOnly    :: Boolean
    , size        :: MantineSize
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

instance ToFFI PinInputType PinInputTypeImpl where
  toNative = case _ of
    PinInputTypeAlphanumeric -> asOneOf "alphanumeric"
    PinInputTypeNumber       -> asOneOf "number"
    PinInputTypeRegExp r     -> asOneOf r

type Props_PinInputImpl =
  Props_CommonImpl
    ( ariaLabel   :: String
    , autoFocus   :: Boolean
    , disabled    :: Boolean
    , error       :: Boolean
    , form        :: String
    , gap         :: MantineSpacingImpl
    , id          :: String
    , inputMode   :: PinInputModeImpl
    , inputType   :: InputTypeImpl
    , length      :: Number
    , manageFocus :: Boolean
    , mask        :: Boolean
    , name        :: String
    , onComplete  :: ValueHandlerImpl String
    , oneTimeCode :: Boolean
    , placeholder :: String
    , radius      :: MantineNumberSizeImpl
    , readOnly    :: Boolean
    , size        :: MantineSizeImpl
    , type        :: PinInputTypeImpl
    | ControlledImpl String
    )
