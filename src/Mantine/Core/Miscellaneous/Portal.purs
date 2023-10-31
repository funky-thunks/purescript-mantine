module Mantine.Core.Miscellaneous.Portal
  ( portal
  , portal_
  , PortalProps
  ) where

import Mantine.Core.Prelude
import Web.HTML.HTMLDivElement (HTMLDivElement)
import Web.HTML.HTMLElement (HTMLElement)

portal :: (PortalProps -> PortalProps) -> JSX
portal = mkComponent portalComponent toNative defaultValue

portal_ :: Array JSX -> JSX
portal_ children = portal _ { children = children }

foreign import portalComponent :: ReactComponent PortalPropsImpl

type PortalProps =
  { children :: Array JSX
  , innerRef :: Maybe (Ref HTMLDivElement)
  , target   :: Maybe HTMLElement
  }

type PortalPropsImpl =
  { children :: Array JSX
  , innerRef :: Nullable (Ref HTMLDivElement)
  , target   :: Nullable HTMLElement
  }
