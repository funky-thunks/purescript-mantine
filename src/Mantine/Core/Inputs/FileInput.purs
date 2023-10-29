module Mantine.Core.Inputs.FileInput
  ( fileInput
  , FileInputProps
  , CaptureMode(..)

  , module Mantine.Core.Inputs.ClearButtonProps
  , module Mantine.Core.Inputs.Input

  , CaptureModeImpl
  ) where

import Mantine.Core.Inputs.ClearButtonProps (ClearButtonProps, ClearButtonPropsImpl)
import Mantine.Core.Inputs.Input (InputVariant(..), InputWrapperOrder(..))
import Mantine.Core.Prelude
import Web.File.File (File)

fileInput :: (FileInputProps -> FileInputProps) -> JSX
fileInput = mkTrivialComponent fileInputComponent

foreign import fileInputComponent :: ReactComponent FileInputPropsImpl

-- Not supported properties
--   { descriptionProps    :: Record<String, any>
--   , errorProps          :: Record<String, any>
--   , fileInputProps      :: Pick<DetailedHTMLProps<InputHTMLAttributes<HTMLInputElement>, HTMLInputElement>, "key" | keyof InputHTMLAttributes<...>>
--   , inputContainer      :: (children: JSX) => JSX
--   , labelProps          :: Record<String, any>
--   , rightSectionProps   :: Record<String, any>
--   , valueComponent      :: FC<{ value: File | File[]; }>
--   , wrapperProps        :: Record<String, any>
--   }

type FileInputProps =
  ThemingProps
    ( accept              :: String
    , capture             :: Maybe CaptureMode
    , clearButtonProps    :: ClearButtonProps
    , clearable           :: Boolean
    , defaultValue        :: Maybe File
    , description         :: Maybe JSX
    , disabled            :: Boolean
    , error               :: Maybe JSX
    , form                :: String
    , icon                :: Maybe JSX
    , iconWidth           :: Maybe Pixels
    , id                  :: Maybe String
    , inputWrapperOrder   :: Maybe (Array InputWrapperOrder)
    , label               :: Maybe JSX
    , multiple            :: Boolean
    , name                :: String
    , onChange            :: ValueHandler File
    , placeholder         :: Maybe String
    , radius              :: Maybe MantineNumberSize
    , readOnly            :: Boolean
    , required            :: Boolean
    , rightSection        :: Maybe JSX
    , rightSectionWidth   :: Maybe Pixels
    , size                :: Maybe MantineSize
    , value               :: Maybe File
    , variant             :: InputVariant
    , withAsterisk        :: Boolean
    )

data CaptureMode = User | Environment | Explicit Boolean

type CaptureModeImpl = Boolean |+| String

instance ToFFI CaptureMode CaptureModeImpl where
  toNative = case _ of
    User        -> asOneOf "user"
    Environment -> asOneOf "environment"
    Explicit b  -> asOneOf b

type FileInputPropsImpl =
  ThemingPropsImpl
    ( accept              :: String
    , capture             :: Nullable CaptureModeImpl
    , clearButtonProps    :: ClearButtonPropsImpl
    , clearable           :: Boolean
    , defaultValue        :: Nullable File
    , description         :: Nullable JSX
    , disabled            :: Boolean
    , error               :: Nullable JSX
    , form                :: String
    , icon                :: Nullable JSX
    , iconWidth           :: Nullable Number
    , id                  :: Nullable String
    , inputWrapperOrder   :: Nullable (Array String)
    , label               :: Nullable JSX
    , multiple            :: Boolean
    , name                :: String
    , onChange            :: EffectFn1 File Unit
    , placeholder         :: Nullable String
    , radius              :: Nullable MantineNumberSizeImpl
    , readOnly            :: Boolean
    , required            :: Boolean
    , rightSection        :: Nullable JSX
    , rightSectionWidth   :: Nullable Number
    , size                :: Nullable String
    , value               :: Nullable File
    , variant             :: String
    , withAsterisk        :: Boolean
    )
