module Mantine.Core.Overlays.LoadingOverlay
  ( loadingOverlay
  , loadingOverlay'
  , Props_LoadingOverlay
  , Props_LoadingOverlayImpl
  ) where

import Mantine.Core.Feedback.Loader (Props_LoaderInner, Props_LoaderInnerImpl)
import Mantine.Core.Overlays.Overlay (Props_OverlayInner, Props_OverlayInnerImpl)
import Mantine.Core.Prelude

loadingOverlay
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_LoadingOverlay
  => Union attrsImpl attrsImpl_ Props_LoadingOverlayImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
loadingOverlay = element (unsafeCoerce loadingOverlayComponent) <<< toNative

loadingOverlay' :: Boolean -> JSX
loadingOverlay' visible = loadingOverlay { visible }

foreign import loadingOverlayComponent :: ReactComponent (Record Props_LoadingOverlayImpl)

type Props_LoadingOverlay =
  Props_Common
    ( loaderProps     :: Record Props_LoaderInner
    , overlayProps    :: Record Props_OverlayInner
    , transitionProps :: MantineTransitionProps
    , visible         :: Boolean
    , zIndex          :: ZIndex
    )

type Props_LoadingOverlayImpl =
  Props_CommonImpl
    ( loaderProps     :: Record Props_LoaderInnerImpl
    , overlayProps    :: Record Props_OverlayInnerImpl
    , transitionProps :: MantineTransitionPropsImpl
    , visible         :: Boolean
    , zIndex          :: ZIndexImpl
    )
