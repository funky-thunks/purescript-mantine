module Mantine.Core.Buttons.FileButton
  ( fileButton
  , multipleFileButton
  , FileButtonProps
  , MandatoryFileButtonProps
  ) where

import Mantine.Core.Inputs.FileInput (CaptureMode, CaptureModeImpl)
import Mantine.Core.Prelude
import Web.File.File (File)

fileButton :: MandatoryFileButtonProps File -> (FileButtonProps File -> FileButtonProps File) -> JSX
fileButton = mkComponent fileButtonComponent fileButtonPropsToImpl <<< defaultMantineComponent

foreign import fileButtonComponent :: ReactComponent (FileButtonPropsImpl File)

multipleFileButton :: MandatoryFileButtonProps (Array File) -> (FileButtonProps (Array File) -> FileButtonProps (Array File)) -> JSX
multipleFileButton = mkComponent multipleFileButtonComponent fileButtonPropsToImpl <<< defaultMantineComponent

foreign import multipleFileButtonComponent :: ReactComponent (FileButtonPropsImpl (Array File))

-- Not supported properties
--   { inputProps :: Pick<DetailedHTMLProps<InputHTMLAttributes<HTMLInputElement>, HTMLInputElement>, "key" | keyof InputHTMLAttributes<...>>
--   }

type MandatoryFileButtonProps payload =
  { accept   :: String
  , children :: { onClick :: Effect Unit } -> JSX
  , onChange :: ValueHandler payload
  }

type FileButtonProps payload =
  MantineComponent
    ( accept   :: String
    , capture  :: Optional CaptureMode
    , children :: { onClick :: Effect Unit } -> JSX
    , disabled :: Boolean
    , form     :: Optional String
    , name     :: Optional String
    , onChange :: ValueHandler payload
    , resetRef :: Optional (Ref (Effect Unit))
    )

type FileButtonPropsImpl payload =
  MantineComponentImpl
    ( accept   :: String
    , capture  :: OptionalImpl CaptureModeImpl
    , children :: { onClick :: Effect Unit } -> JSX
    , disabled :: Boolean
    , form     :: OptionalImpl String
    , name     :: OptionalImpl String
    , onChange :: ValueHandlerImpl payload
    , resetRef :: OptionalImpl (Ref (Effect Unit))
    )

fileButtonPropsToImpl :: forall payload. FromFFI payload payload => FileButtonProps payload -> FileButtonPropsImpl payload
fileButtonPropsToImpl props =
  let rest = toNative <<< delete (Proxy :: Proxy "children")
   in { children: props.children } `union` rest props
