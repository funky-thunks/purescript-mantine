module Mantine.Core.Inputs.Checkables
  ( Props_CheckableComponent
  , Props_CheckableComponentImpl

  , Props_CheckableFieldComponent
  , Props_CheckableFieldComponentImpl

  , CheckableLabelPosition(..)
  , CheckableLabelPositionImpl
  ) where

import Mantine.Core.Prelude
import Web.HTML.HTMLDivElement (HTMLDivElement)

type Props_CheckableComponent rest =
  Props_Common
    ( checked        :: Boolean
    , color          :: MantineColor
    , defaultChecked :: Boolean
    , id             :: String
    , onChange       :: CheckerHandler
    , radius         :: MantineNumberSize
    , rootRef        :: Ref HTMLDivElement
    , size           :: MantineSize
    , value          :: String
    | rest
    )

type Props_CheckableFieldComponent rest =
  Props_CheckableComponent
    ( description   :: JSX
    , error         :: JSX
    , label         :: JSX
    , labelPosition :: CheckableLabelPosition
    | rest
    )

type Props_CheckableComponentImpl rest =
  Props_CommonImpl
    ( checked        :: Boolean
    , color          :: MantineColorImpl
    , defaultChecked :: Boolean
    , id             :: String
    , onChange       :: CheckerHandlerImpl
    , radius         :: MantineNumberSizeImpl
    , rootRef        :: Ref HTMLDivElement
    , size           :: MantineSizeImpl
    , value          :: String
    | rest
    )

type Props_CheckableFieldComponentImpl rest =
  Props_CheckableComponentImpl
    ( description   :: JSX
    , error         :: JSX
    , label         :: JSX
    , labelPosition :: CheckableLabelPositionImpl
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
