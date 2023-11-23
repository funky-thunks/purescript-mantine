module Mantine.Core.Overlays.Overlay
  ( overlay
  , Props_Overlay

  , Props_OverlayImpl
  ) where

import Mantine.Core.Prelude

overlay
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Overlay
  => Union attrsImpl attrsImpl_ Props_OverlayImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
overlay = element (unsafeCoerce overlayComponent) <<< toNative

foreign import overlayComponent :: ReactComponent (Record Props_OverlayImpl)

type Props_Overlay =
  Props_Common
    ( backgroundOpacity :: Number
    , blur              :: Int
    , center            :: Boolean
    , children          :: Array JSX
    , color             :: MantineColor
    , fixed             :: Boolean
    , gradient          :: String
    , radius            :: MantineNumberSize
    , zIndex            :: ZIndex
    )

type Props_OverlayImpl =
  Props_CommonImpl
    ( backgroundOpacity :: Number
    , blur              :: Number
    , center            :: Boolean
    , children          :: Array JSX
    , color             :: MantineColorImpl
    , fixed             :: Boolean
    , gradient          :: String
    , radius            :: MantineNumberSizeImpl
    , zIndex            :: ZIndexImpl
    )
