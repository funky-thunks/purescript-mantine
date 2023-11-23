module Mantine.Core.Buttons.FileButton
  ( fileButton
  , multipleFileButton
  , Props_FileButton
  , Props_FileButtonImpl
  ) where

import Mantine.Core.Inputs.FileInput (CaptureMode, CaptureModeImpl)
import Mantine.Core.Prelude
import Web.File.File (File)

fileButton
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     (Props_FileButton     File)
  => Union attrsImpl attrsImpl_ (Props_FileButtonImpl File)
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
fileButton = element (unsafeCoerce fileButtonComponent) <<< toNative

foreign import fileButtonComponent :: ReactComponent (Record (Props_FileButtonImpl File))

multipleFileButton
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     (Props_FileButton     (Array File))
  => Union attrsImpl attrsImpl_ (Props_FileButtonImpl (Array File))
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
multipleFileButton = element (unsafeCoerce multipleFileButtonComponent) <<< toNative

foreign import multipleFileButtonComponent :: ReactComponent (Record (Props_FileButtonImpl (Array File)))

-- Not supported properties
--   { inputProps :: Pick<DetailedHTMLProps<InputHTMLAttributes<HTMLInputElement>, HTMLInputElement>, "key" | keyof InputHTMLAttributes<...>>
--   }

type Props_FileButton payload =
  Props_Common
    ( accept   :: String
    , capture  :: CaptureMode
    , children :: { onClick :: Effect Unit } -> JSX
    , disabled :: Boolean
    , form     :: String
    , name     :: String
    , onChange :: ValueHandler payload
    , resetRef :: Ref (Effect Unit)
    )

type Props_FileButtonImpl payload =
  Props_CommonImpl
    ( accept   :: String
    , capture  :: CaptureModeImpl
    , children :: { onClick :: Effect Unit } -> JSX
    , disabled :: Boolean
    , form     :: String
    , name     :: String
    , onChange :: ValueHandlerImpl payload
    , resetRef :: Ref (Effect Unit)
    )
