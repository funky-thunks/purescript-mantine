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
    ( color         :: Optional MantineColor
    , label         :: Optional JSX
    , labelPosition :: Optional DividerLabelPosition
    , orientation   :: Orientation
    , size          :: Optional MantineNumberSize
    , variant       :: Optional DividerVariant
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
    ( color         :: OptionalImpl MantineColorImpl
    , label         :: OptionalImpl JSX
    , labelPosition :: OptionalImpl DividerLabelPositionImpl
    , orientation   :: OrientationImpl
    , size          :: OptionalImpl MantineNumberSizeImpl
    , variant       :: OptionalImpl DividerVariantImpl
    )
