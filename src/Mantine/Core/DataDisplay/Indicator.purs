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
  ThemingProps
    ( children      :: Array JSX
    , color         :: Maybe MantineColor
    , disabled      :: Boolean
    , dot           :: Boolean
    , inline        :: Boolean
    , label         :: Maybe JSX
    , offset        :: Maybe Number
    , overflowCount :: Maybe Number
    , position      :: IndicatorPosition
    , processing    :: Boolean
    , radius        :: Maybe MantineNumberSize
    , showZero      :: Boolean
    , size          :: Maybe Pixels
    , withBorder    :: Boolean
    , zIndex        :: Maybe Number
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

instance ToFFI IndicatorPosition String where
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
  ThemingPropsImpl
    ( children      :: Array JSX
    , color         :: Nullable String
    , disabled      :: Boolean
    , dot           :: Boolean
    , inline        :: Boolean
    , label         :: Nullable JSX
    , offset        :: Nullable Number
    , overflowCount :: Nullable Number
    , position      :: String
    , processing    :: Boolean
    , radius        :: Nullable MantineNumberSizeImpl
    , showZero      :: Boolean
    , size          :: Nullable Number
    , withBorder    :: Boolean
    , zIndex        :: Nullable Number
    )
