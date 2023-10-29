module Mantine.Core.Miscellaneous.Collapse
  ( collapse
  , collapse_
  , CollapseProps
  ) where

import Mantine.Core.Prelude

collapse :: (CollapseProps -> CollapseProps) -> JSX
collapse = mkComponent collapseComponent collapseToImpl defaultCollapseProps

collapse_ :: Array JSX -> JSX
collapse_ children = collapse _ { children = children }

foreign import collapseComponent :: ReactComponent CollapsePropsImpl

type CollapseProps =
  ThemingProps
    ( animateOpacity           :: Boolean
    , children                 :: Array JSX
    , onTransitionEnd          :: Effect Unit
    , opened                   :: Boolean
    , transitionDuration       :: Number
    , transitionTimingFunction :: MantineTransitionTimingFunction
    )

defaultCollapseProps :: CollapseProps
defaultCollapseProps =
  defaultThemingProps
    { animateOpacity:           true
    , onTransitionEnd:          pure unit
    , transitionDuration:       200.0
    , transitionTimingFunction: TransitionTimingEase
    }

type CollapsePropsImpl =
  ThemingPropsImpl
    ( animateOpacity           :: Boolean
    , children                 :: Array JSX
    , in                       :: Boolean
    , onTransitionEnd          :: Effect Unit
    , transitionDuration       :: Number
    , transitionTimingFunction :: String
    )

collapseToImpl :: CollapseProps -> CollapsePropsImpl
collapseToImpl = toNative <<< rename (Proxy :: Proxy "opened") (Proxy :: Proxy "in")
