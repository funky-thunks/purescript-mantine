module Mantine.Core.Overlays.LoadingOverlay
  ( loadingOverlay
  , loadingOverlay'
  , LoadingOverlayProps
  ) where

import Mantine.Core.Prelude

loadingOverlay :: (LoadingOverlayProps -> LoadingOverlayProps) -> JSX
loadingOverlay = mkTrivialComponent loadingOverlayComponent

loadingOverlay' :: Boolean -> JSX
loadingOverlay' visible = loadingOverlay _ { visible = visible }

foreign import loadingOverlayComponent :: ReactComponent LoadingOverlayPropsImpl

type LoadingOverlayProps =
  ThemingProps
    ( exitTransitionDuration :: Maybe Milliseconds
    , loader                 :: Maybe JSX
    , overlayBlur            :: Maybe Number
    , overlayColor           :: Maybe String
    , overlayOpacity         :: Maybe Number
    , radius                 :: Maybe MantineNumberSize
    , transitionDuration     :: Maybe Milliseconds
    , visible                :: Boolean
    , zIndex                 :: Maybe Number
    )

type LoadingOverlayPropsImpl =
  ThemingPropsImpl
    ( exitTransitionDuration :: Nullable Number
    , loader                 :: Nullable JSX
    , overlayBlur            :: Nullable Number
    , overlayColor           :: Nullable String
    , overlayOpacity         :: Nullable Number
    , radius                 :: Nullable MantineNumberSizeImpl
    , transitionDuration     :: Nullable Number
    , visible                :: Boolean
    , zIndex                 :: Nullable Number
    )
