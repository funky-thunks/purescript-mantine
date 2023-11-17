module Mantine.Core.DataDisplay.Kbd
  ( kbd
  , kbd_
  , Props_Kbd
  , Props_KbdImpl
  ) where

import Mantine.Core.Prelude
import React.Basic.DOM as DOM

kbd
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Kbd
  => Union attrsImpl attrsImpl_ Props_KbdImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
kbd = element (unsafeCoerce kbdComponent) <<< toNative

kbd_ :: String -> JSX
kbd_ text = kbd { children: [ DOM.text text ] }

foreign import kbdComponent :: ReactComponent (Record Props_KbdImpl)

type Props_Kbd =
  ( children :: Array JSX
  , size     :: MantineSize
  )

type Props_KbdImpl =
  ( children :: Array JSX
  , size     :: MantineSizeImpl
  )
