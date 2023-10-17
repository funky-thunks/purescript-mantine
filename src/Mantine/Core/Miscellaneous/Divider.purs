module Mantine.Core.Miscellaneous.Divider
  ( divider
  , divider_
  , DividerProps
  , DividerLabelPosition(..)
  , DividerVariant(..)
  ) where

import Prelude (identity)
import Mantine.Core.Prelude

divider :: (DividerProps -> DividerProps) -> JSX
divider = mkComponentWithDefault dividerComponent defaultDividerProps

divider_ :: JSX
divider_ = divider identity

foreign import dividerComponent :: ReactComponent DividerPropsImpl

type DividerProps =
  ThemingProps
    ( color         :: Maybe MantineColor
    , label         :: Maybe JSX
    , labelPosition :: Maybe DividerLabelPosition
    , orientation   :: Orientation
    , size          :: Maybe MantineNumberSize
    , variant       :: Maybe DividerVariant
    )

defaultDividerProps :: DividerProps
defaultDividerProps =
  defaultThemingProps
    { orientation: Horizontal
    } `union` defaultValue

data DividerLabelPosition
  = DividerLabelPositionLeft
  | DividerLabelPositionCenter
  | DividerLabelPositionRight

instance ToFFI DividerLabelPosition String where
  toNative = case _ of
    DividerLabelPositionLeft   -> "left"
    DividerLabelPositionCenter -> "center"
    DividerLabelPositionRight  -> "right"

data DividerVariant
  = DividerVariantDashed
  | DividerVariantDotted
  | DividerVariantSolid

instance ToFFI DividerVariant String where
  toNative = case _ of
    DividerVariantDashed -> "dashed"
    DividerVariantDotted -> "dotted"
    DividerVariantSolid  -> "solid"

type DividerPropsImpl =
  ThemingPropsImpl
    ( color         :: Nullable String
    , label         :: Nullable JSX
    , labelPosition :: Nullable String
    , orientation   :: String
    , size          :: Nullable MantineNumberSizeImpl
    , variant       :: Nullable String
    )
