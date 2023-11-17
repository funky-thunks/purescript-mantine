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
    ( backgroundOpacity :: Optional Number
    , blur              :: Optional Number
    , center            :: Boolean
    , children          :: Array JSX
    , color             :: Optional MantineColor
    , fixed             :: Boolean
    , gradient          :: Optional String
    , radius            :: Optional MantineNumberSize
    , zIndex            :: Optional ZIndex
    )

type OverlayPropsImpl =
  MantineComponentImpl
    ( backgroundOpacity :: OptionalImpl Number
    , blur              :: OptionalImpl Number
    , center            :: Boolean
    , children          :: Array JSX
    , color             :: OptionalImpl MantineColorImpl
    , fixed             :: Boolean
    , gradient          :: OptionalImpl String
    , radius            :: OptionalImpl MantineNumberSizeImpl
    , zIndex            :: OptionalImpl ZIndexImpl
    )
