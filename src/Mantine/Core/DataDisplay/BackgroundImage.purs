module Mantine.Core.DataDisplay.BackgroundImage
  ( backgroundImage
  , backgroundImage_
  , Props_BackgroundImage
  , Props_BackgroundImageImpl
  ) where

import Mantine.Core.Prelude

backgroundImage
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_BackgroundImage
  => Union attrsImpl attrsImpl_ Props_BackgroundImageImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
backgroundImage = element (unsafeCoerce backgroundImageComponent) <<< toNative

backgroundImage_ :: String -> JSX
backgroundImage_ src = backgroundImage { src }

foreign import backgroundImageComponent:: ReactComponent (Record Props_BackgroundImageImpl)

type Props_BackgroundImage =
  Props_Common
    ( radius :: MantineNumberSize
    , src    :: String
    )

type Props_BackgroundImageImpl =
  Props_CommonImpl
    ( radius :: MantineNumberSizeImpl
    , src    :: String
    )
