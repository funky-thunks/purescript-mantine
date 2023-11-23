module Mantine.Core.Navigation.Stepper
  ( stepper
  , Props_Stepper
  , Props_StepperImpl
  , StepClickHandler(..)
  , StepClickHandlerImpl
  , StepFragmentComponent(..)
  , StepFragmentComponentImpl
  , StepperIconPosition(..)
  , StepperIconPositionImpl

  , stepperStep
  , Props_StepperStep
  , Props_StepperStepImpl
  , StepState(..)
  , StepStateImpl

  , stepperCompleted_
  , Props_StepperCompleted    
  , Props_StepperCompletedImpl
  ) where

import Data.Int (floor)
import Effect.Uncurried (mkEffectFn1)
import Mantine.Core.Prelude

stepper
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Stepper
  => Union attrsImpl attrsImpl_ Props_StepperImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
stepper = element (unsafeCoerce stepperComponent) <<< toNative

foreign import stepperComponent :: ReactComponent (Record Props_StepperImpl)

type Props_Stepper =
  Props_Common
    ( active               :: Number
    , allowNextStepsSelect :: Boolean
    , children             :: Array JSX
    , color                :: MantineColor
    , completedIcon        :: StepFragmentComponent
    , contentPadding       :: MantineNumberSize
    , icon                 :: StepFragmentComponent
    , iconPosition         :: StepperIconPosition
    , iconSize             :: Pixels
    , onStepClick          :: StepClickHandler
    , orientation          :: Orientation
    , progressIcon         :: StepFragmentComponent
    , radius               :: MantineNumberSize
    , size                 :: MantineSize
    , wrap                 :: Boolean
    )

newtype StepClickHandler = StepClickHandler (Int -> Effect Unit)

type StepClickHandlerImpl = EffectFn1 Number Unit

instance ToFFI StepClickHandler StepClickHandlerImpl where
  toNative (StepClickHandler sch) = mkEffectFn1 (sch <<< floor)

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

type StepperIconPositionImpl = String

instance ToFFI StepperIconPosition StepperIconPositionImpl where
  toNative = case _ of
    StepperIconPositionLeft  -> "left"
    StepperIconPositionRight -> "right"

type Props_StepperImpl =
  Props_CommonImpl
    ( active               :: Number
    , allowNextStepsSelect :: Boolean
    , children             :: Array JSX
    , color                :: MantineColorImpl
    , completedIcon        :: StepFragmentComponentImpl
    , contentPadding       :: MantineNumberSizeImpl
    , icon                 :: StepFragmentComponentImpl
    , iconPosition         :: StepperIconPositionImpl
    , iconSize             :: PixelsImpl
    , onStepClick          :: StepClickHandlerImpl
    , orientation          :: OrientationImpl
    , progressIcon         :: StepFragmentComponentImpl
    , radius               :: MantineNumberSizeImpl
    , size                 :: MantineSizeImpl
    , wrap                 :: Boolean
    )

stepperStep
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_StepperStep
  => Union attrsImpl attrsImpl_ Props_StepperStepImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
stepperStep = element (unsafeCoerce stepperStepComponent) <<< toNative

foreign import stepperStepComponent :: ReactComponent (Record Props_StepperStepImpl)

type Props_StepperStep =
  Props_Common
    ( allowStepClick  :: Boolean
    , allowStepSelect :: Boolean
    , children        :: Array JSX
    , color           :: MantineColor
    , completedIcon   :: StepFragmentComponent
    , description     :: StepFragmentComponent
    , icon            :: StepFragmentComponent
    , iconPosition    :: StepperIconPosition
    , iconSize        :: Pixels
    , label           :: StepFragmentComponent
    , loading         :: Boolean
    , orientation     :: Orientation
    , progressIcon    :: StepFragmentComponent
    , state           :: StepState
    , step            :: Number
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

type Props_StepperStepImpl =
  Props_CommonImpl
    ( allowStepClick  :: Boolean
    , allowStepSelect :: Boolean
    , children        :: Array JSX
    , color           :: MantineColorImpl
    , completedIcon   :: StepFragmentComponentImpl
    , description     :: StepFragmentComponentImpl
    , icon            :: StepFragmentComponentImpl
    , iconPosition    :: StepperIconPositionImpl
    , iconSize        :: PixelsImpl
    , label           :: StepFragmentComponentImpl
    , loading         :: Boolean
    , orientation     :: OrientationImpl
    , progressIcon    :: StepFragmentComponentImpl
    , state           :: StepStateImpl
    , step            :: Number
    , withIcon        :: Boolean
    )

stepperCompleted
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_StepperCompleted
  => Union attrsImpl attrsImpl_ Props_StepperCompletedImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
stepperCompleted = element (unsafeCoerce stepperCompletedComponent) <<< toNative

stepperCompleted_ :: Array JSX -> JSX
stepperCompleted_ children = stepperCompleted { children }

foreign import stepperCompletedComponent :: ReactComponent (Record Props_StepperCompletedImpl)

type Props_StepperCompleted     = Props_Common     ( children :: Array JSX )
type Props_StepperCompletedImpl = Props_CommonImpl ( children :: Array JSX )
