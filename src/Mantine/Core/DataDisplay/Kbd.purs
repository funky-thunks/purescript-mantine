module Mantine.Core.DataDisplay.Kbd
  ( kbd
  ) where

import Mantine.Core.Prelude
import React.Basic (element)
import React.Basic.DOM as DOM

kbd :: String -> JSX
kbd text = element kbdComponent { children: [ DOM.text text ] }

foreign import kbdComponent :: ReactComponent { children :: Array JSX }
