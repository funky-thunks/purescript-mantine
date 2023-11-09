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
    , color      :: Maybe MantineColor
    , disabled   :: Boolean
    , inline     :: Boolean
    , label      :: Maybe JSX
    , offset     :: Maybe Pixels
    , position   :: IndicatorPosition
    , processing :: Boolean
    , radius     :: Maybe MantineNumberSize
    , size       :: Maybe Pixels
    , withBorder :: Boolean
    , zIndex     :: Maybe ZIndex
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
    , color      :: Nullable MantineColorImpl
    , disabled   :: Boolean
    , inline     :: Boolean
    , label      :: Nullable JSX
    , offset     :: Nullable PixelsImpl
    , position   :: IndicatorPositionImpl
    , processing :: Boolean
    , radius     :: Nullable MantineNumberSizeImpl
    , size       :: Nullable PixelsImpl
    , withBorder :: Boolean
    , zIndex     :: Nullable ZIndexImpl
    )
