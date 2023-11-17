module Mantine.Core.Layout.AspectRatio
  ( aspectRatio
  , Props_AspectRatio
  , Props_AspectRatioImpl
  ) where

import Mantine.Core.Prelude

aspectRatio
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_AspectRatio
  => Union attrsImpl attrsImpl_ Props_AspectRatioImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
aspectRatio = element (unsafeCoerce aspectRatioComponent) <<< toNative

foreign import aspectRatioComponent :: ReactComponent (Record Props_AspectRatioImpl)

type Props_AspectRatio =
  Props_Common
    ( children :: Array JSX
    , ratio    :: Number
    )

type Props_AspectRatioImpl =
  Props_CommonImpl
    ( children :: Array JSX
    , ratio    :: Number
    )
