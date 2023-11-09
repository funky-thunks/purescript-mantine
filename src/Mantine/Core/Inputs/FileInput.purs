module Mantine.Core.Inputs.FileInput
  ( fileInput
  , FileInputProps
  , CaptureMode(..)

  , module Mantine.Core.Inputs.ClearButtonProps

  , CaptureModeImpl
  ) where

import Mantine.Core.Inputs.ClearButtonProps (ClearButtonProps, ClearButtonPropsImpl)
import Mantine.Core.Inputs.Input (InputComponent, InputComponentImpl)
import Mantine.Core.Prelude
import Web.File.File (File)

fileInput :: (FileInputProps -> FileInputProps) -> JSX
fileInput = mkTrivialComponent fileInputComponent

foreign import fileInputComponent :: ReactComponent FileInputPropsImpl

-- Not supported properties
--   { fileInputProps :: Omit<DetailedHTMLProps<InputHTMLAttributes<HTMLInputElement>, HTMLInputElement>, "ref">
--   , valueComponent :: FC<{ value: File | File[] | null; }>
--   }

type FileInputProps =
  InputComponent
    ( accept           :: String
    , capture          :: Maybe CaptureMode
    , clearButtonProps :: ClearButtonProps
    , clearable        :: Boolean
    , form             :: String
    , multiple         :: Boolean
    , name             :: String
    , readOnly         :: Boolean
    | Controlled File
    )

data CaptureMode = User | Environment | Explicit Boolean

type CaptureModeImpl = Boolean |+| String

instance ToFFI CaptureMode CaptureModeImpl where
  toNative = case _ of
    User        -> asOneOf "user"
    Environment -> asOneOf "environment"
    Explicit b  -> asOneOf b

type FileInputPropsImpl =
  InputComponentImpl
    ( accept           :: String
    , capture          :: Nullable CaptureModeImpl
    , clearButtonProps :: ClearButtonPropsImpl
    , clearable        :: Boolean
    , form             :: String
    , multiple         :: Boolean
    , name             :: String
    , readOnly         :: Boolean
    | ControlledImpl File
    )
