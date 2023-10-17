module Mantine.Core.Miscellaneous.Collapse
  ( collapse
  , collapse_
  , CollapseProps
  , CollapseAxis(..)
  ) where

import Prelude (Unit, unit, pure, (<<<))
import Mantine.Core.Prelude

collapse :: (CollapseProps -> CollapseProps) -> JSX
collapse = mkComponent collapseComponent collapseToImpl defaultCollapseProps

collapse_ :: Array JSX -> JSX
collapse_ children = collapse _ { children = children }

foreign import collapseComponent :: ReactComponent CollapsePropsImpl

type CollapseProps =
  ThemingProps
    ( children                 :: Array JSX
    , animateOpacity           :: Boolean
    , axis                     :: CollapseAxis
    , onTransitionEnd          :: Effect Unit
    , opened                   :: Boolean
    , transitionDuration       :: Number
    , transitionTimingFunction :: MantineTransitionTimingFunction
    )

data CollapseAxis
  = CollapseAxisX
  | CollapseAxisY

instance ToFFI CollapseAxis String where
  toNative = case _ of
    CollapseAxisX -> "x"
    CollapseAxisY -> "y"

instance DefaultValue CollapseAxis where defaultValue = CollapseAxisY

defaultCollapseProps :: CollapseProps
defaultCollapseProps =
  defaultThemingProps
    { animateOpacity:           true
    , onTransitionEnd:          pure unit
    , transitionDuration:       200.0
    , transitionTimingFunction: TransitionTimingEase
    } `union` defaultValue

type CollapsePropsImpl =
  ThemingPropsImpl
    ( children                 :: Array JSX
    , animateOpacity           :: Boolean
    , axis                     :: String
    , in                       :: Boolean
    , onTransitionEnd          :: Effect Unit
    , transitionDuration       :: Number
    , transitionTimingFunction :: String
    )

collapseToImpl :: CollapseProps -> CollapsePropsImpl
collapseToImpl = toNative <<< rename (Proxy :: Proxy "opened") (Proxy :: Proxy "in")
