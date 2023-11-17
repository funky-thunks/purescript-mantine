module Mantine.Core.Miscellaneous.Collapse
  ( collapse
  , collapse_
  , Props_Collapse
  , Props_CollapseImpl
  ) where

import Mantine.Core.Prelude

collapse
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Collapse
  => Union attrsImpl attrsImpl_ Props_CollapseImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
collapse = element (unsafeCoerce collapseComponent) <<< toNative

collapse_ :: Array JSX -> JSX
collapse_ children = collapse { children }

foreign import collapseComponent :: ReactComponent (Record Props_CollapseImpl)

type Props_Collapse =
  Props_Common
    ( animateOpacity           :: Boolean
    , children                 :: Array JSX
    , in                       :: Boolean
    , onTransitionEnd          :: Effect Unit
    , transitionDuration       :: Milliseconds
    , transitionTimingFunction :: MantineTransitionTimingFunction
    )

type Props_CollapseImpl =
  Props_CommonImpl
    ( animateOpacity           :: Boolean
    , children                 :: Array JSX
    , in                       :: Boolean
    , onTransitionEnd          :: Effect Unit
    , transitionDuration       :: MillisecondsImpl
    , transitionTimingFunction :: MantineTransitionTimingFunctionImpl
    )
