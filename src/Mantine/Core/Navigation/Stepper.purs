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
stepper = mkComponent stepperComponent stepperPropsToImpl defaultThemingProps_

foreign import stepperComponent :: ReactComponent StepperPropsImpl

type StepperProps =
  ThemingProps
    ( active               :: Maybe Number
    , allowNextStepsSelect :: Boolean
    , breakpoint           :: Maybe MantineNumberSize
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
    )

data StepFragmentComponent = Constant JSX
                           | StepDependent (Int -> JSX)

foreign import stepFragmentComponent :: (Int -> JSX) -> ReactComponent { step :: Number }

type StepFragmentComponentImpl = JSX |+| ReactComponent { step :: Number }

instance ToFFI StepFragmentComponent StepFragmentComponentImpl where
  toNative = case _ of
    Constant      jsx     -> asOneOf jsx
    StepDependent builder -> asOneOf (stepFragmentComponent builder)

data StepperIconPosition = StepperIconPositionLeft | StepperIconPositionRight

instance DefaultValue StepperIconPosition where defaultValue = StepperIconPositionLeft

instance ToFFI StepperIconPosition String where
  toNative = case _ of
    StepperIconPositionLeft  -> "left"
    StepperIconPositionRight -> "right"

type StepperPropsImpl =
  ThemingPropsImpl
    ( active               :: Nullable Number
    , allowNextStepsSelect :: Boolean
    , breakpoint           :: Nullable MantineNumberSizeImpl
    , children             :: Array JSX
    , color                :: Nullable String
    , completedIcon        :: Nullable StepFragmentComponentImpl
    , contentPadding       :: Nullable MantineNumberSizeImpl
    , icon                 :: Nullable StepFragmentComponentImpl
    , iconPosition         :: String
    , iconSize             :: Nullable Number
    , onStepClick          :: EffectFn1 Number Unit
    , orientation          :: Nullable String
    , progressIcon         :: Nullable StepFragmentComponentImpl
    , radius               :: Nullable MantineNumberSizeImpl
    , size                 :: Nullable String
    )

stepperPropsToImpl :: StepperProps -> StepperPropsImpl
stepperPropsToImpl props = toNative $ props { onStepClick = floor >$< props.onStepClick }

stepperStep :: (StepperStepProps -> StepperStepProps) -> JSX
stepperStep = mkTrivialComponent stepperStepComponent

foreign import stepperStepComponent :: ReactComponent StepperStepPropsImpl

type StepperStepProps =
  ThemingProps
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
    , radius          :: Maybe MantineNumberSize
    , size            :: Maybe MantineSize
    , state           :: Maybe StepState
    , step            :: Maybe Number
    , withIcon        :: Boolean
    )

data StepState = StepStateInactive | StepStateProgress | StepStateCompleted

instance ToFFI StepState String where
  toNative = case _ of
    StepStateInactive  -> "stepInactive"
    StepStateProgress  -> "stepProgress"
    StepStateCompleted -> "stepCompleted"

type StepperStepPropsImpl =
  ThemingPropsImpl
    ( allowStepClick  :: Boolean
    , allowStepSelect :: Boolean
    , children        :: Array JSX
    , color           :: Nullable String
    , completedIcon   :: Nullable StepFragmentComponentImpl
    , description     :: Nullable StepFragmentComponentImpl
    , icon            :: Nullable StepFragmentComponentImpl
    , iconPosition    :: String
    , iconSize        :: Nullable Number
    , label           :: Nullable StepFragmentComponentImpl
    , loading         :: Boolean
    , orientation     :: Nullable String
    , progressIcon    :: Nullable StepFragmentComponentImpl
    , radius          :: Nullable MantineNumberSizeImpl
    , size            :: Nullable String
    , state           :: Nullable String
    , step            :: Nullable Number
    , withIcon        :: Boolean
    )

stepperCompleted :: (StepperCompletedProps -> StepperCompletedProps) -> JSX
stepperCompleted = mkTrivialComponent stepperCompletedComponent

stepperCompleted_ :: Array JSX -> JSX
stepperCompleted_ children = stepperCompleted _ { children = children }

foreign import stepperCompletedComponent :: ReactComponent StepperCompletedPropsImpl

type StepperCompletedProps     = ThemingProps     ( children :: Array JSX )
type StepperCompletedPropsImpl = ThemingPropsImpl ( children :: Array JSX )
