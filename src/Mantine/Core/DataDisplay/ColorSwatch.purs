module Mantine.Core.DataDisplay.ColorSwatch
  ( colorSwatch
  , colorSwatch_
  , Props_ColorSwatch
  , Props_ColorSwatchImpl
  ) where

import Mantine.Core.Prelude

colorSwatch
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_ColorSwatch
  => Union attrsImpl attrsImpl_ Props_ColorSwatchImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
colorSwatch = element (unsafeCoerce colorSwatchComponent) <<< toNative

colorSwatch_ :: MantineColor -> JSX
colorSwatch_ color = colorSwatch { color }

foreign import colorSwatchComponent :: ReactComponent (Record Props_ColorSwatchImpl)

type Props_ColorSwatch =
  Props_Common
    ( children   :: Array JSX
    , color      :: MantineColor
    , radius     :: MantineNumberSize
    , size       :: Pixels
    , withShadow :: Boolean
    )

type Props_ColorSwatchImpl =
  Props_CommonImpl
    ( children   :: Array JSX
    , color      :: MantineColorImpl
    , radius     :: MantineNumberSizeImpl
    , size       :: PixelsImpl
    , withShadow :: Boolean
    )
