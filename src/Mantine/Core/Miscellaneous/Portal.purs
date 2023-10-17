module Mantine.Core.Miscellaneous.Portal
  ( portal_
  ) where

import React.Basic (JSX, ReactComponent, element)

portal_ :: Array JSX -> JSX
portal_ children = element portalComponent { children }

foreign import portalComponent :: ReactComponent { children :: Array JSX }
