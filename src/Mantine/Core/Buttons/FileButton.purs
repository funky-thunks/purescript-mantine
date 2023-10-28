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
fileButton = mkComponent fileButtonComponent fileButtonPropsToImpl <<< defaultThemingProps

foreign import fileButtonComponent :: ReactComponent (FileButtonPropsImpl File)

multipleFileButton :: MandatoryFileButtonProps (Array File) -> (FileButtonProps (Array File) -> FileButtonProps (Array File)) -> JSX
multipleFileButton = mkComponent multipleFileButtonComponent fileButtonPropsToImpl <<< defaultThemingProps

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
  ThemingProps
    ( accept   :: String
    , capture  :: Maybe CaptureMode
    , children :: { onClick :: Effect Unit } -> JSX
    , disabled :: Boolean
    , form     :: Maybe String
    , multiple :: Boolean
    , name     :: Maybe String
    , onChange :: ValueHandler payload
    , resetRef :: Maybe (Ref (Effect Unit))
    )

type FileButtonPropsImpl payload =
  ThemingPropsImpl
    ( accept   :: String
    , capture  :: Nullable CaptureModeImpl
    , children :: { onClick :: Effect Unit } -> JSX
    , disabled :: Boolean
    , form     :: Nullable String
    , multiple :: Boolean
    , name     :: Nullable String
    , onChange :: EffectFn1 payload Unit
    , resetRef :: Nullable (Ref (Effect Unit))
    )

fileButtonPropsToImpl :: forall payload. FromFFI payload payload => FileButtonProps payload -> FileButtonPropsImpl payload
fileButtonPropsToImpl props =
  let rest = toNative <<< delete (Proxy :: Proxy "children")
   in { children: props.children } `union` rest props
