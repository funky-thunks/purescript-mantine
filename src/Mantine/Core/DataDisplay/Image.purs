module Mantine.Core.DataDisplay.Image
  ( image
  , Props_Image
  , Props_ImageImpl
  ) where

import Mantine.Core.Prelude

image
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Image
  => Union attrsImpl attrsImpl_ Props_ImageImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
image = element (unsafeCoerce imageComponent) <<< toNative

foreign import imageComponent :: ReactComponent (Record Props_ImageImpl)

type Props_Image =
  Props_Common
    ( fallbackSrc :: String
    , fit         :: ObjectFit
    , onError     :: EventHandler
    , radius      :: MantineNumberSize
    , src         :: String
    )

type Props_ImageImpl =
  Props_CommonImpl
    ( fallbackSrc :: String
    , fit         :: ObjectFitImpl
    , onError     :: EventHandler
    , radius      :: MantineNumberSizeImpl
    , src         :: String
    )
