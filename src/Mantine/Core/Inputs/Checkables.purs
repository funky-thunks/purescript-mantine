module Mantine.Core.Inputs.Checkables
  ( CheckableComponent
  , CheckableComponentImpl

  , CheckableLabelPosition(..)
  ) where

import Mantine.Core.Prelude
import Web.HTML.HTMLDivElement (HTMLDivElement)

type CheckableComponent rest =
  ThemingProps
    ( checked       :: Maybe Boolean
    , color         :: Maybe MantineColor
    , description   :: Maybe JSX
    , error         :: Maybe JSX
    , id            :: Maybe String
    , label         :: Maybe JSX
    , labelPosition :: Maybe CheckableLabelPosition
    , onChange      :: CheckerHandler
    , radius        :: Maybe MantineNumberSize
    , rootRef       :: Maybe (Ref HTMLDivElement)
    , size          :: Maybe MantineSize
    , value         :: Maybe String
    | rest
    )

type CheckableComponentImpl rest =
  ThemingPropsImpl
    ( checked       :: Nullable Boolean
    , color         :: Nullable String
    , description   :: Nullable JSX
    , error         :: Nullable JSX
    , id            :: Nullable String
    , label         :: Nullable JSX
    , labelPosition :: Nullable String
    , onChange      :: EffectFn1 SyntheticEvent Unit
    , radius        :: Nullable MantineNumberSizeImpl
    , rootRef       :: Nullable (Ref HTMLDivElement)
    , size          :: Nullable String
    , value         :: Nullable String
    | rest
    )

data CheckableLabelPosition
  = CheckableLabelPositionLeft
  | CheckableLabelPositionRight

instance ToFFI CheckableLabelPosition String where
  toNative = case _ of
    CheckableLabelPositionLeft  -> "left"
    CheckableLabelPositionRight -> "right"
