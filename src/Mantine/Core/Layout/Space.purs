module Mantine.Core.Layout.Space
  ( space
  , SpaceProps
  ) where

import Mantine.Core.Prelude

space :: (SpaceProps -> SpaceProps) -> JSX
space = mkTrivialComponent spaceComponent

foreign import spaceComponent :: ReactComponent SpacePropsImpl

type SpaceProps     = MantineComponent     ()
type SpacePropsImpl = MantineComponentImpl ()
