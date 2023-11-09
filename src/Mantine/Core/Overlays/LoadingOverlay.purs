module Mantine.Core.Overlays.LoadingOverlay
  ( loadingOverlay
  , loadingOverlay'
  , LoadingOverlayProps
  ) where

import Mantine.Core.Feedback.Loader (LoaderProps, LoaderPropsImpl)
import Mantine.Core.Overlays.Overlay (OverlayProps, OverlayPropsImpl)
import Mantine.Core.Prelude

loadingOverlay :: (LoadingOverlayProps -> LoadingOverlayProps) -> JSX
loadingOverlay = mkTrivialComponent loadingOverlayComponent

loadingOverlay' :: Boolean -> JSX
loadingOverlay' visible = loadingOverlay _ { visible = visible }

foreign import loadingOverlayComponent :: ReactComponent LoadingOverlayPropsImpl

type LoadingOverlayProps =
  MantineComponent
    ( loaderProps     :: LoaderProps
    , overlayProps    :: OverlayProps
    , transitionProps :: MantineTransitionProps
    , visible         :: Boolean
    , zIndex          :: Maybe ZIndex
    )

type LoadingOverlayPropsImpl =
  MantineComponentImpl
    ( loaderProps     :: LoaderPropsImpl
    , overlayProps    :: OverlayPropsImpl
    , transitionProps :: MantineTransitionPropsImpl
    , visible         :: Boolean
    , zIndex          :: Nullable ZIndexImpl
    )
