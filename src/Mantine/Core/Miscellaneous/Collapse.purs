module Mantine.Core.Miscellaneous.Collapse
  ( collapse
  , collapse_
  , CollapseProps
  ) where

import Mantine.Core.Prelude

collapse :: (CollapseProps -> CollapseProps) -> JSX
collapse = mkComponentWithDefault collapseComponent defaultCollapseProps

collapse_ :: Array JSX -> JSX
collapse_ children = collapse _ { children = children }

foreign import collapseComponent :: ReactComponent CollapsePropsImpl

type CollapseProps =
  MantineComponent
    ( animateOpacity           :: Boolean
    , children                 :: Array JSX
    , in                       :: Boolean
    , onTransitionEnd          :: Effect Unit
    , transitionDuration       :: Milliseconds
    , transitionTimingFunction :: MantineTransitionTimingFunction
    )

defaultCollapseProps :: CollapseProps
defaultCollapseProps =
  defaultMantineComponent
    { animateOpacity:           true
    , onTransitionEnd:          pure unit
    , transitionDuration:       200.0
    , transitionTimingFunction: TransitionTimingEase
    }

type CollapsePropsImpl =
  MantineComponentImpl
    ( animateOpacity           :: Boolean
    , children                 :: Array JSX
    , in                       :: Boolean
    , onTransitionEnd          :: Effect Unit
    , transitionDuration       :: MillisecondsImpl
    , transitionTimingFunction :: MantineTransitionTimingFunctionImpl
    )
