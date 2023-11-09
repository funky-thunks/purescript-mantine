module Mantine.Core.Overlays.Overlay
  ( overlay
  , OverlayProps

  , OverlayPropsImpl
  ) where

import Mantine.Core.Prelude

overlay :: (OverlayProps -> OverlayProps) -> JSX
overlay = mkComponent overlayComponent toNative defaultMantineComponent_

foreign import overlayComponent :: ReactComponent OverlayPropsImpl

type OverlayProps =
  MantineComponent
    ( backgroundOpacity :: Maybe Number
    , blur              :: Maybe Number
    , center            :: Boolean
    , children          :: Array JSX
    , color             :: Maybe MantineColor
    , fixed             :: Boolean
    , gradient          :: Maybe String
    , radius            :: Maybe MantineNumberSize
    , zIndex            :: Maybe ZIndex
    )

type OverlayPropsImpl =
  MantineComponentImpl
    ( backgroundOpacity :: Nullable Number
    , blur              :: Nullable Number
    , center            :: Boolean
    , children          :: Array JSX
    , color             :: Nullable MantineColorImpl
    , fixed             :: Boolean
    , gradient          :: Nullable String
    , radius            :: Nullable MantineNumberSizeImpl
    , zIndex            :: Nullable ZIndexImpl
    )
