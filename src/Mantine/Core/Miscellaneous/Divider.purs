module Mantine.Core.Miscellaneous.Divider
  ( divider
  , divider_
  , Props_Divider
  , Props_DividerImpl
  , DividerLabelPosition(..)
  , DividerLabelPositionImpl
  , DividerVariant(..)
  , DividerVariantImpl
  ) where

import Mantine.Core.Prelude

divider
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Divider
  => Union attrsImpl attrsImpl_ Props_DividerImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
divider = element (unsafeCoerce dividerComponent) <<< toNative

divider_ :: JSX
divider_ = divider {}

foreign import dividerComponent :: ReactComponent (Record Props_DividerImpl)

type Props_Divider =
  Props_Common
    ( color         :: MantineColor
    , label         :: JSX
    , labelPosition :: DividerLabelPosition
    , orientation   :: Orientation
    , size          :: MantineNumberSize
    , variant       :: DividerVariant
    )

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

type Props_DividerImpl =
  Props_CommonImpl
    ( color         :: MantineColorImpl
    , label         :: JSX
    , labelPosition :: DividerLabelPositionImpl
    , orientation   :: OrientationImpl
    , size          :: MantineNumberSizeImpl
    , variant       :: DividerVariantImpl
    )
