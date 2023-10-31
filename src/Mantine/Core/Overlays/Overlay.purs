module Mantine.Core.Overlays.Overlay
  ( overlay
  , OverlayProps
  ) where

import Mantine.Core.Prelude

overlay :: (OverlayProps -> OverlayProps) -> JSX
overlay = mkComponent overlayComponent toNative defaultThemingProps_

foreign import overlayComponent :: ReactComponent OverlayPropsImpl

type OverlayProps =
  ThemingProps
    ( blur     :: Maybe Number
    , center   :: Boolean
    , children :: Array JSX
    , color    :: Maybe MantineColor
    , fixed    :: Boolean
    , gradient :: Maybe String
    , radius   :: Maybe MantineNumberSize
    , zIndex   :: Maybe Number
    )

type OverlayPropsImpl =
  ThemingPropsImpl
    ( blur     :: Nullable Number
    , center   :: Boolean
    , children :: Array JSX
    , color    :: Nullable String
    , fixed    :: Boolean
    , gradient :: Nullable String
    , radius   :: Nullable MantineNumberSizeImpl
    , zIndex   :: Nullable Number
    )
