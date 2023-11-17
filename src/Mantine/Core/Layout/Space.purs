module Mantine.Core.Layout.Space
  ( space
  , Props_Space
  , Props_SpaceImpl
  ) where

import Mantine.Core.Prelude

space
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Space
  => Union attrsImpl attrsImpl_ Props_SpaceImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
space = element (unsafeCoerce spaceComponent) <<< toNative

foreign import spaceComponent :: ReactComponent (Record Props_SpaceImpl)

type Props_Space     = Props_Common     ()
type Props_SpaceImpl = Props_CommonImpl ()
