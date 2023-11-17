module Mantine.Core.DataDisplay.Kbd
  ( kbd
  , kbd_
  , KbdProps
  ) where

import Mantine.Core.Prelude
import React.Basic.DOM as DOM

kbd :: (KbdProps -> KbdProps) -> JSX
kbd = mkComponentWithDefault kbdComponent defaultValue

kbd_ :: String -> JSX
kbd_ text = kbd _ { children = [ DOM.text text ] }

foreign import kbdComponent :: ReactComponent KbdPropsImpl

type KbdProps =
  { children :: Array JSX
  , size     :: Optional MantineSize
  }

type KbdPropsImpl =
  { children :: Array JSX
  , size     :: OptionalImpl MantineSizeImpl
  }
