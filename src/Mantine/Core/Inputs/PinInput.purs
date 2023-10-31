module Mantine.Core.Inputs.PinInput
  ( pinInput
  , PinInputProps
  , PinInputMode(..)
  , PinInputType(..)
  , PinInputVariant(..)

  , module Mantine.Core.Inputs.Input
  ) where

import Data.String.Regex (Regex)
import Mantine.Core.Inputs.Input (InputType(..))
import Mantine.Core.Prelude

pinInput :: (PinInputProps -> PinInputProps) -> JSX
pinInput = mkTrivialComponent pinInputComponent

foreign import pinInputComponent :: ReactComponent PinInputPropsImpl

-- Not supported properties:
--   { rightSectionProps :: Record<String, any>
--   , wrapperProps      :: Record<String, any>
--   }

type PinInputProps =
  ThemingProps
    ( autoFocus         :: Boolean
    , defaultValue      :: Maybe String
    , disabled          :: Boolean
    , error             :: Boolean
    , form              :: Maybe String
    , icon              :: Maybe JSX
    , iconWidth         :: Maybe Pixels
    , id                :: Maybe String
    , inputMode         :: Maybe PinInputMode
    , inputType         :: Maybe InputType
    , length            :: Number
    , manageFocus       :: Boolean
    , mask              :: Boolean
    , name              :: Maybe String
    , onChange          :: ValueHandler String
    , onComplete        :: ValueHandler String
    , oneTimeCode       :: Boolean
    , placeholder       :: Maybe String
    , radius            :: Maybe MantineNumberSize
    , readOnly          :: Boolean
    , required          :: Boolean
    , rightSection      :: Maybe JSX
    , rightSectionWidth :: Maybe Pixels
    , size              :: Maybe MantineSize
    , spacing           :: Maybe MantineNumberSize
    , type              :: PinInputType
    , value             :: Maybe String
    , variant           :: Maybe PinInputVariant
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

data PinInputVariant
  = PinInputVariantUnstyled
  | PinInputVariantDefault
  | PinInputVariantFilled

instance ToFFI PinInputVariant String where
  toNative = case _ of
    PinInputVariantUnstyled -> "unstyled"
    PinInputVariantDefault  -> "default"
    PinInputVariantFilled   -> "filled"

type PinInputPropsImpl =
  ThemingPropsImpl
    ( autoFocus         :: Boolean
    , defaultValue      :: Nullable String
    , disabled          :: Boolean
    , error             :: Boolean
    , form              :: Nullable String
    , icon              :: Nullable JSX
    , iconWidth         :: Nullable Number
    , id                :: Nullable String
    , inputMode         :: Nullable String
    , inputType         :: Nullable String
    , length            :: Number
    , manageFocus       :: Boolean
    , mask              :: Boolean
    , name              :: Nullable String
    , onChange          :: EffectFn1 String Unit
    , onComplete        :: EffectFn1 String Unit
    , oneTimeCode       :: Boolean
    , placeholder       :: Nullable String
    , radius            :: Nullable MantineNumberSizeImpl
    , readOnly          :: Boolean
    , required          :: Boolean
    , rightSection      :: Nullable JSX
    , rightSectionWidth :: Nullable Number
    , size              :: Nullable String
    , spacing           :: Nullable MantineNumberSizeImpl
    , type              :: PinInputTypeImpl
    , value             :: Nullable String
    , variant           :: Nullable String
    )
