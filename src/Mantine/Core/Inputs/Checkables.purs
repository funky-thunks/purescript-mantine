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
    ( checked        :: Optional Boolean
    , color          :: Optional MantineColor
    , defaultChecked :: Optional Boolean
    , id             :: Optional String
    , onChange       :: CheckerHandler
    , radius         :: Optional MantineNumberSize
    , rootRef        :: Optional (Ref HTMLDivElement)
    , size           :: Optional MantineSize
    , value          :: Optional String
    | rest
    )

type CheckableFieldComponent rest =
  CheckableComponent
    ( description    :: Optional JSX
    , error          :: Optional JSX
    , label          :: Optional JSX
    , labelPosition  :: Optional CheckableLabelPosition
    | rest
    )

type CheckableComponentImpl rest =
  MantineComponentImpl
    ( checked        :: OptionalImpl Boolean
    , color          :: OptionalImpl MantineColorImpl
    , defaultChecked :: OptionalImpl Boolean
    , id             :: OptionalImpl String
    , onChange       :: CheckerHandlerImpl
    , radius         :: OptionalImpl MantineNumberSizeImpl
    , rootRef        :: OptionalImpl (Ref HTMLDivElement)
    , size           :: OptionalImpl MantineSizeImpl
    , value          :: OptionalImpl String
    | rest
    )

type CheckableFieldComponentImpl rest =
  CheckableComponentImpl
    ( description    :: OptionalImpl JSX
    , error          :: OptionalImpl JSX
    , label          :: OptionalImpl JSX
    , labelPosition  :: OptionalImpl CheckableLabelPositionImpl
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
