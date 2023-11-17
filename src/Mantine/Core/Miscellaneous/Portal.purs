module Mantine.Core.Miscellaneous.Portal
  ( portal
  , portal_
  , PortalProps
  , PortalComponent
  , PortalTarget(..)

  , optionalPortal
  , optionalPortal_
  , OptionalPortalProps
  ) where

import Mantine.Core.Prelude
import Web.HTML.HTMLDivElement (HTMLDivElement)
import Web.HTML.HTMLElement (HTMLElement)

portal :: (PortalProps -> PortalProps) -> JSX
portal = mkComponent portalComponent toNative defaultValue

portal_ :: Array JSX -> JSX
portal_ children = portal _ { children = children }

foreign import portalComponent :: ReactComponent PortalPropsImpl

type PortalProps     = PortalComponent     ()
type PortalPropsImpl = PortalComponentImpl ()

optionalPortal :: (OptionalPortalProps -> OptionalPortalProps) -> JSX
optionalPortal = mkComponent optionalPortalComponent toNative defaultValue

optionalPortal_ :: Array JSX -> JSX
optionalPortal_ children = optionalPortal _ { children = children }

foreign import optionalPortalComponent :: ReactComponent OptionalPortalPropsImpl

type OptionalPortalProps     = PortalComponent     (withinPortal :: Boolean)
type OptionalPortalPropsImpl = PortalComponentImpl (withinPortal :: Boolean)

type PortalComponent rest =
  { children :: Array JSX
  , ref      :: Optional (Ref HTMLDivElement)
  , target   :: Optional PortalTarget
  | rest
  }

data PortalTarget
  = PortalTargetSelector String
  | PortalTargetElement  HTMLElement

type PortalTargetImpl = String |+| HTMLElement

instance ToFFI PortalTarget PortalTargetImpl where
  toNative = case _ of
    PortalTargetSelector s -> asOneOf s
    PortalTargetElement  e -> asOneOf e

type PortalComponentImpl rest =
  { children :: Array JSX
  , ref      :: OptionalImpl (Ref HTMLDivElement)
  , target   :: OptionalImpl PortalTargetImpl
  | rest
  }
