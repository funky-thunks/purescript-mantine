module Mantine.Core.DataDisplay.Indicator
  ( indicator
  , Props_Indicator
  , Props_IndicatorImpl
  , IndicatorPosition(..)
  , IndicatorPositionImpl
  ) where

import Mantine.Core.Prelude

indicator
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Indicator
  => Union attrsImpl attrsImpl_ Props_IndicatorImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
indicator = element (unsafeCoerce indicatorComponent) <<< toNative

foreign import indicatorComponent :: ReactComponent (Record Props_IndicatorImpl)

type Props_Indicator =
  Props_Common
    ( children   :: Array JSX
    , color      :: MantineColor
    , disabled   :: Boolean
    , inline     :: Boolean
    , label      :: JSX
    , offset     :: Pixels
    , position   :: IndicatorPosition
    , processing :: Boolean
    , radius     :: MantineNumberSize
    , size       :: Pixels
    , withBorder :: Boolean
    , zIndex     :: ZIndex
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

type Props_IndicatorImpl =
  Props_CommonImpl
    ( children   :: Array JSX
    , color      :: MantineColorImpl
    , disabled   :: Boolean
    , inline     :: Boolean
    , label      :: JSX
    , offset     :: PixelsImpl
    , position   :: IndicatorPositionImpl
    , processing :: Boolean
    , radius     :: MantineNumberSizeImpl
    , size       :: PixelsImpl
    , withBorder :: Boolean
    , zIndex     :: ZIndexImpl
    )
