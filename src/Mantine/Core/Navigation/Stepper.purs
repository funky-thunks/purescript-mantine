module Mantine.Core.Navigation.Stepper
  ( stepper
  , StepperProps
  , StepFragmentComponent(..)
  , StepperIconPosition(..)

  , stepperStep
  , StepperStepProps
  , StepState(..)

  , stepperCompleted_
  ) where

import Data.Functor.Contravariant ((>$<))
import Data.Int (floor)
import Mantine.Core.Prelude

stepper :: (StepperProps -> StepperProps) -> JSX
stepper = mkComponent stepperComponent stepperPropsToImpl defaultStepperProps

foreign import stepperComponent :: ReactComponent StepperPropsImpl

type StepperProps =
  MantineComponent
    ( active               :: Optional Number
    , allowNextStepsSelect :: Boolean
    , children             :: Array JSX
    , color                :: Optional MantineColor
    , completedIcon        :: Optional StepFragmentComponent
    , contentPadding       :: Optional MantineNumberSize
    , icon                 :: Optional StepFragmentComponent
    , iconPosition         :: StepperIconPosition
    , iconSize             :: Optional Pixels
    , onStepClick          :: ValueHandler Int
    , orientation          :: Optional Orientation
    , progressIcon         :: Optional StepFragmentComponent
    , radius               :: Optional MantineNumberSize
    , size                 :: Optional MantineSize
    , wrap                 :: Boolean
    )

defaultStepperProps :: StepperProps
defaultStepperProps = { wrap: true } `union` defaultValue

data StepFragmentComponent = Constant JSX
                           | StepDependent (Int -> JSX)

foreign import stepFragmentComponent :: (Int -> JSX) -> ReactComponent { step :: Number }

type StepFragmentComponentImpl = JSX |+| ReactComponent { step :: Number }

instance ToFFI StepFragmentComponent StepFragmentComponentImpl where
  toNative = case _ of
    Constant      jsx     -> asOneOf jsx
    StepDependent builder -> asOneOf (stepFragmentComponent builder)

data StepperIconPosition
  = StepperIconPositionLeft
  | StepperIconPositionRight

instance DefaultValue StepperIconPosition where
  defaultValue = StepperIconPositionLeft

type StepperIconPositionImpl = String

instance ToFFI StepperIconPosition StepperIconPositionImpl where
  toNative = case _ of
    StepperIconPositionLeft  -> "left"
    StepperIconPositionRight -> "right"

type StepperPropsImpl =
  MantineComponentImpl
    ( active               :: OptionalImpl Number
    , allowNextStepsSelect :: Boolean
    , children             :: Array JSX
    , color                :: OptionalImpl MantineColorImpl
    , completedIcon        :: OptionalImpl StepFragmentComponentImpl
    , contentPadding       :: OptionalImpl MantineNumberSizeImpl
    , icon                 :: OptionalImpl StepFragmentComponentImpl
    , iconPosition         :: StepperIconPositionImpl
    , iconSize             :: OptionalImpl PixelsImpl
    , onStepClick          :: ValueHandlerImpl Number
    , orientation          :: OptionalImpl OrientationImpl
    , progressIcon         :: OptionalImpl StepFragmentComponentImpl
    , radius               :: OptionalImpl MantineNumberSizeImpl
    , size                 :: OptionalImpl MantineSizeImpl
    , wrap                 :: Boolean
    )

stepperPropsToImpl :: StepperProps -> StepperPropsImpl
stepperPropsToImpl props = toNative $ props { onStepClick = floor >$< props.onStepClick }

stepperStep :: (StepperStepProps -> StepperStepProps) -> JSX
stepperStep = mkTrivialComponent stepperStepComponent

foreign import stepperStepComponent :: ReactComponent StepperStepPropsImpl

type StepperStepProps =
  MantineComponent
    ( allowStepClick  :: Boolean
    , allowStepSelect :: Boolean
    , children        :: Array JSX
    , color           :: Optional MantineColor
    , completedIcon   :: Optional StepFragmentComponent
    , description     :: Optional StepFragmentComponent
    , icon            :: Optional StepFragmentComponent
    , iconPosition    :: StepperIconPosition
    , iconSize        :: Optional Pixels
    , label           :: Optional StepFragmentComponent
    , loading         :: Boolean
    , orientation     :: Optional Orientation
    , progressIcon    :: Optional StepFragmentComponent
    , state           :: Optional StepState
    , step            :: Optional Number
    , withIcon        :: Boolean
    )

data StepState
  = StepStateInactive
  | StepStateProgress
  | StepStateCompleted

type StepStateImpl = String

instance ToFFI StepState StepStateImpl where
  toNative = case _ of
    StepStateInactive  -> "stepInactive"
    StepStateProgress  -> "stepProgress"
    StepStateCompleted -> "stepCompleted"

type StepperStepPropsImpl =
  MantineComponentImpl
    ( allowStepClick  :: Boolean
    , allowStepSelect :: Boolean
    , children        :: Array JSX
    , color           :: OptionalImpl MantineColorImpl
    , completedIcon   :: OptionalImpl StepFragmentComponentImpl
    , description     :: OptionalImpl StepFragmentComponentImpl
    , icon            :: OptionalImpl StepFragmentComponentImpl
    , iconPosition    :: StepperIconPositionImpl
    , iconSize        :: OptionalImpl PixelsImpl
    , label           :: OptionalImpl StepFragmentComponentImpl
    , loading         :: Boolean
    , orientation     :: OptionalImpl OrientationImpl
    , progressIcon    :: OptionalImpl StepFragmentComponentImpl
    , state           :: OptionalImpl StepStateImpl
    , step            :: OptionalImpl Number
    , withIcon        :: Boolean
    )

stepperCompleted :: (StepperCompletedProps -> StepperCompletedProps) -> JSX
stepperCompleted = mkTrivialComponent stepperCompletedComponent

stepperCompleted_ :: Array JSX -> JSX
stepperCompleted_ children = stepperCompleted _ { children = children }

foreign import stepperCompletedComponent :: ReactComponent StepperCompletedPropsImpl

type StepperCompletedProps     = MantineComponent     ( children :: Array JSX )
type StepperCompletedPropsImpl = MantineComponentImpl ( children :: Array JSX )
