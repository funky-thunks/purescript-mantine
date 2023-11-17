module Mantine.Core.DataDisplay.Indicator
  ( indicator
  , IndicatorProps
  , IndicatorPosition(..)
  ) where

import Mantine.Core.Prelude

indicator :: (IndicatorProps -> IndicatorProps) -> JSX
indicator = mkTrivialComponent indicatorComponent

foreign import indicatorComponent :: ReactComponent IndicatorPropsImpl

type IndicatorProps =
  MantineComponent
    ( children   :: Array JSX
    , color      :: Optional MantineColor
    , disabled   :: Boolean
    , inline     :: Boolean
    , label      :: Optional JSX
    , offset     :: Optional Pixels
    , position   :: IndicatorPosition
    , processing :: Boolean
    , radius     :: Optional MantineNumberSize
    , size       :: Optional Pixels
    , withBorder :: Boolean
    , zIndex     :: Optional ZIndex
    )

data IndicatorPosition
  = BottomEnd
  | BottomStart
  | TopEnd
  | TopStart
  | BottomCenter
  | TopCenter
  | MiddleCenter
  | MiddleEnd
  | MiddleStart

instance DefaultValue IndicatorPosition where defaultValue = TopEnd

type IndicatorPositionImpl = String

instance ToFFI IndicatorPosition IndicatorPositionImpl where
  toNative = case _ of
    BottomEnd    -> "bottom-end"
    BottomStart  -> "bottom-start"
    TopEnd       -> "top-end"
    TopStart     -> "top-start"
    BottomCenter -> "bottom-center"
    TopCenter    -> "top-center"
    MiddleCenter -> "middle-center"
    MiddleEnd    -> "middle-end"
    MiddleStart  -> "middle-start"

type IndicatorPropsImpl =
  MantineComponentImpl
    ( children   :: Array JSX
    , color      :: OptionalImpl MantineColorImpl
    , disabled   :: Boolean
    , inline     :: Boolean
    , label      :: OptionalImpl JSX
    , offset     :: OptionalImpl PixelsImpl
    , position   :: IndicatorPositionImpl
    , processing :: Boolean
    , radius     :: OptionalImpl MantineNumberSizeImpl
    , size       :: OptionalImpl PixelsImpl
    , withBorder :: Boolean
    , zIndex     :: OptionalImpl ZIndexImpl
    )
