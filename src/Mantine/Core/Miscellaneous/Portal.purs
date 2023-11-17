module Mantine.Core.Miscellaneous.Portal
  ( portal
  , portal_
  , Props_Portal
  , Props_PortalImpl
  , Props_PortalComponent
  , Props_PortalComponentImpl
  , PortalTarget(..)
  , PortalTargetImpl

  , optionalPortal
  , optionalPortal_
  , Props_OptionalPortal
  , Props_OptionalPortalImpl
  ) where

import Mantine.Core.Prelude
import Web.HTML.HTMLDivElement (HTMLDivElement)
import Web.HTML.HTMLElement (HTMLElement)

portal
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Portal
  => Union attrsImpl attrsImpl_ Props_PortalImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
portal = element (unsafeCoerce portalComponent) <<< toNative

portal_ :: Array JSX -> JSX
portal_ children = portal { children }

foreign import portalComponent :: ReactComponent (Record Props_PortalImpl)

type Props_Portal     = Props_PortalComponent     ()
type Props_PortalImpl = Props_PortalComponentImpl ()

optionalPortal
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_OptionalPortal
  => Union attrsImpl attrsImpl_ Props_OptionalPortalImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
optionalPortal = element (unsafeCoerce optionalPortalComponent) <<< toNative

optionalPortal_ :: Array JSX -> JSX
optionalPortal_ children = optionalPortal { children }

foreign import optionalPortalComponent :: ReactComponent (Record Props_OptionalPortalImpl)

type Props_OptionalPortal     = Props_PortalComponent     (withinPortal :: Boolean)
type Props_OptionalPortalImpl = Props_PortalComponentImpl (withinPortal :: Boolean)

type Props_PortalComponent rest =
  ( children :: Array JSX
  , ref      :: Ref HTMLDivElement
  , target   :: PortalTarget
  | rest
  )

data PortalTarget
  = PortalTargetSelector String
  | PortalTargetElement  HTMLElement

type PortalTargetImpl = String |+| HTMLElement

instance ToFFI PortalTarget PortalTargetImpl where
  toNative = case _ of
    PortalTargetSelector s -> asOneOf s
    PortalTargetElement  e -> asOneOf e

type Props_PortalComponentImpl rest =
  ( children :: Array JSX
  , ref      :: Ref HTMLDivElement
  , target   :: PortalTargetImpl
  | rest
  )
