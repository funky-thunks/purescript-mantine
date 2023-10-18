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
    , color    :: Maybe MantineColor
    , gradient :: Maybe String
    , radius   :: Maybe MantineNumberSize
    , zIndex   :: Maybe Number
    )

type OverlayPropsImpl =
  ThemingPropsImpl
    ( blur     :: Nullable Number
    , color    :: Nullable String
    , gradient :: Nullable String
    , radius   :: Nullable MantineNumberSizeImpl
    , zIndex   :: Nullable Number
    )
