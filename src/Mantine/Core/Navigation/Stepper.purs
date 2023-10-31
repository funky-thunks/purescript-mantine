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
    ( active               :: Maybe Number
    , allowNextStepsSelect :: Boolean
    , children             :: Array JSX
    , color                :: Maybe MantineColor
    , completedIcon        :: Maybe StepFragmentComponent
    , contentPadding       :: Maybe MantineNumberSize
    , icon                 :: Maybe StepFragmentComponent
    , iconPosition         :: StepperIconPosition
    , iconSize             :: Maybe Pixels
    , onStepClick          :: ValueHandler Int
    , orientation          :: Maybe Orientation
    , progressIcon         :: Maybe StepFragmentComponent
    , radius               :: Maybe MantineNumberSize
    , size                 :: Maybe MantineSize
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
    ( active               :: Nullable Number
    , allowNextStepsSelect :: Boolean
    , children             :: Array JSX
    , color                :: Nullable MantineColorImpl
    , completedIcon        :: Nullable StepFragmentComponentImpl
    , contentPadding       :: Nullable MantineNumberSizeImpl
    , icon                 :: Nullable StepFragmentComponentImpl
    , iconPosition         :: StepperIconPositionImpl
    , iconSize             :: Nullable PixelsImpl
    , onStepClick          :: ValueHandlerImpl Number
    , orientation          :: Nullable OrientationImpl
    , progressIcon         :: Nullable StepFragmentComponentImpl
    , radius               :: Nullable MantineNumberSizeImpl
    , size                 :: Nullable MantineSizeImpl
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
    , color           :: Maybe MantineColor
    , completedIcon   :: Maybe StepFragmentComponent
    , description     :: Maybe StepFragmentComponent
    , icon            :: Maybe StepFragmentComponent
    , iconPosition    :: StepperIconPosition
    , iconSize        :: Maybe Pixels
    , label           :: Maybe StepFragmentComponent
    , loading         :: Boolean
    , orientation     :: Maybe Orientation
    , progressIcon    :: Maybe StepFragmentComponent
    , state           :: Maybe StepState
    , step            :: Maybe Number
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
    , color           :: Nullable MantineColorImpl
    , completedIcon   :: Nullable StepFragmentComponentImpl
    , description     :: Nullable StepFragmentComponentImpl
    , icon            :: Nullable StepFragmentComponentImpl
    , iconPosition    :: StepperIconPositionImpl
    , iconSize        :: Nullable PixelsImpl
    , label           :: Nullable StepFragmentComponentImpl
    , loading         :: Boolean
    , orientation     :: Nullable OrientationImpl
    , progressIcon    :: Nullable StepFragmentComponentImpl
    , state           :: Nullable StepStateImpl
    , step            :: Nullable Number
    , withIcon        :: Boolean
    )

stepperCompleted :: (StepperCompletedProps -> StepperCompletedProps) -> JSX
stepperCompleted = mkTrivialComponent stepperCompletedComponent

stepperCompleted_ :: Array JSX -> JSX
stepperCompleted_ children = stepperCompleted _ { children = children }

foreign import stepperCompletedComponent :: ReactComponent StepperCompletedPropsImpl

type StepperCompletedProps     = MantineComponent     ( children :: Array JSX )
type StepperCompletedPropsImpl = MantineComponentImpl ( children :: Array JSX )
