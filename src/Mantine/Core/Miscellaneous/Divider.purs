module Mantine.Core.Miscellaneous.Divider
  ( divider
  , divider_
  , DividerProps
  , DividerLabelPosition(..)
  , DividerVariant(..)
  ) where

import Mantine.Core.Prelude

divider :: (DividerProps -> DividerProps) -> JSX
divider = mkComponentWithDefault dividerComponent defaultDividerProps

divider_ :: JSX
divider_ = divider identity

foreign import dividerComponent :: ReactComponent DividerPropsImpl

type DividerProps =
  MantineComponent
    ( color         :: Maybe MantineColor
    , label         :: Maybe JSX
    , labelPosition :: Maybe DividerLabelPosition
    , orientation   :: Orientation
    , size          :: Maybe MantineNumberSize
    , variant       :: Maybe DividerVariant
    )

defaultDividerProps :: DividerProps
defaultDividerProps = defaultMantineComponent { orientation: Horizontal }

data DividerLabelPosition
  = DividerLabelPositionLeft
  | DividerLabelPositionCenter
  | DividerLabelPositionRight

type DividerLabelPositionImpl = String

instance ToFFI DividerLabelPosition DividerLabelPositionImpl where
  toNative = case _ of
    DividerLabelPositionLeft   -> "left"
    DividerLabelPositionCenter -> "center"
    DividerLabelPositionRight  -> "right"

data DividerVariant
  = DividerVariantDashed
  | DividerVariantDotted
  | DividerVariantSolid

type DividerVariantImpl = String

instance ToFFI DividerVariant DividerVariantImpl where
  toNative = case _ of
    DividerVariantDashed -> "dashed"
    DividerVariantDotted -> "dotted"
    DividerVariantSolid  -> "solid"

type DividerPropsImpl =
  MantineComponentImpl
    ( color         :: Nullable MantineColorImpl
    , label         :: Nullable JSX
    , labelPosition :: Nullable DividerLabelPositionImpl
    , orientation   :: OrientationImpl
    , size          :: Nullable MantineNumberSizeImpl
    , variant       :: Nullable DividerVariantImpl
    )
