module Mantine.Core.Miscellaneous.Box
  ( box
  , BoxProps
  ) where

import Mantine.Core.Prelude

box :: (BoxProps -> BoxProps) -> JSX
box = mkTrivialComponent boxComponent

foreign import boxComponent :: ReactComponent BoxPropsImpl

type BoxProps = ThemingProps (children :: Array JSX)

type BoxPropsImpl = ThemingPropsImpl (children :: Array JSX)
