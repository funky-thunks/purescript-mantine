module Mantine.Core.Miscellaneous.Box
  ( box
  , Props_Box
  , Props_BoxImpl
  ) where

import Mantine.Core.Prelude

box
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Box
  => Union attrsImpl attrsImpl_ Props_BoxImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
box = element (unsafeCoerce boxComponent) <<< toNative

foreign import boxComponent :: ReactComponent (Record Props_BoxImpl)

type Props_Box     = Props_Common     ( children :: Array JSX )
type Props_BoxImpl = Props_CommonImpl ( children :: Array JSX )
