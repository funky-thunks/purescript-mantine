module Mantine.Core.Miscellaneous.FocusTrap
  ( focusTrap
  , focusTrap_
  , Props_FocusTrap
  ) where

import Mantine.Core.Prelude

focusTrap
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_FocusTrap
  => Union attrsImpl attrsImpl_ Props_FocusTrap
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
focusTrap = element (unsafeCoerce focusTrapComponent) <<< toNative

focusTrap_ :: Array JSX -> JSX
focusTrap_ children = focusTrap { children }

foreign import focusTrapComponent :: ReactComponent (Record Props_FocusTrap)

type Props_FocusTrap =
  ( active   :: Boolean
  , children :: Array JSX
  , refProp  :: String
  )
