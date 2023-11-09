module Mantine.Core.Inputs.Checkables
  ( CheckableComponent
  , CheckableFieldComponent
  , CheckableLabelPosition(..)

  , CheckableComponentImpl
  , CheckableFieldComponentImpl
  , CheckableLabelPositionImpl
  ) where

import Mantine.Core.Prelude
import Web.HTML.HTMLDivElement (HTMLDivElement)

type CheckableComponent rest =
  MantineComponent
    ( checked        :: Maybe Boolean
    , color          :: Maybe MantineColor
    , defaultChecked :: Maybe Boolean
    , id             :: Maybe String
    , onChange       :: CheckerHandler
    , radius         :: Maybe MantineNumberSize
    , rootRef        :: Maybe (Ref HTMLDivElement)
    , size           :: Maybe MantineSize
    , value          :: Maybe String
    | rest
    )

type CheckableFieldComponent rest =
  CheckableComponent
    ( description    :: Maybe JSX
    , error          :: Maybe JSX
    , label          :: Maybe JSX
    , labelPosition  :: Maybe CheckableLabelPosition
    | rest
    )

type CheckableComponentImpl rest =
  MantineComponentImpl
    ( checked        :: Nullable Boolean
    , color          :: Nullable MantineColorImpl
    , defaultChecked :: Nullable Boolean
    , id             :: Nullable String
    , onChange       :: CheckerHandlerImpl
    , radius         :: Nullable MantineNumberSizeImpl
    , rootRef        :: Nullable (Ref HTMLDivElement)
    , size           :: Nullable MantineSizeImpl
    , value          :: Nullable String
    | rest
    )

type CheckableFieldComponentImpl rest =
  CheckableComponentImpl
    ( description    :: Nullable JSX
    , error          :: Nullable JSX
    , label          :: Nullable JSX
    , labelPosition  :: Nullable CheckableLabelPositionImpl
    | rest
    )

data CheckableLabelPosition
  = CheckableLabelPositionLeft
  | CheckableLabelPositionRight

type CheckableLabelPositionImpl = String

instance ToFFI CheckableLabelPosition CheckableLabelPositionImpl where
  toNative = case _ of
    CheckableLabelPositionLeft  -> "left"
    CheckableLabelPositionRight -> "right"
