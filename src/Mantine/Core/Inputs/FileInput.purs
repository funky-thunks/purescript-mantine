module Mantine.Core.Inputs.FileInput
  ( fileInput
  , Props_FileInput
  , Props_FileInputImpl
  , CaptureMode(..)
  , CaptureModeImpl

  , module Mantine.Core.Inputs.ClearButtonProps
  ) where

import Mantine.Core.Inputs.ClearButtonProps (ClearButtonProps, ClearButtonPropsImpl)
import Mantine.Core.Inputs.Input (Props_InputComponent, Props_InputComponentImpl)
import Mantine.Core.Prelude
import Web.File.File (File)

fileInput
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_FileInput
  => Union attrsImpl attrsImpl_ Props_FileInputImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
fileInput = element (unsafeCoerce fileInputComponent) <<< toNative

foreign import fileInputComponent :: ReactComponent (Record Props_FileInputImpl)

-- Not supported properties
--   { fileInputProps :: Omit<DetailedHTMLProps<InputHTMLAttributes<HTMLInputElement>, HTMLInputElement>, "ref">
--   , valueComponent :: FC<{ value: File | File[] | null; }>
--   }

type Props_FileInput =
  Props_InputComponent
    ( accept           :: String
    , capture          :: CaptureMode
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

type Props_FileInputImpl =
  Props_InputComponentImpl
    ( accept           :: String
    , capture          :: CaptureModeImpl
    , clearButtonProps :: ClearButtonPropsImpl
    , clearable        :: Boolean
    , form             :: String
    , multiple         :: Boolean
    , name             :: String
    , readOnly         :: Boolean
    | ControlledImpl File
    )
