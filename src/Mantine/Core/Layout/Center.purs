module Mantine.Core.Layout.Center
  ( center
  , center_
  , Props_Center
  , Props_CenterImpl
  ) where

import Mantine.Core.Prelude

center
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Center
  => Union attrsImpl attrsImpl_ Props_CenterImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
center = element (unsafeCoerce centerComponent) <<< toNative

center_ :: Array JSX -> JSX
center_ children = center { children }

foreign import centerComponent :: ReactComponent (Record Props_CenterImpl)

type Props_Center =
  Props_Common
    ( children :: Array JSX
    , inline   :: Boolean
    )

type Props_CenterImpl =
  Props_CommonImpl
    ( children :: Array JSX
    , inline   :: Boolean
    )
