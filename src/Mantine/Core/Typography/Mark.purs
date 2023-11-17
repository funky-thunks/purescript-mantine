module Mantine.Core.Typography.Mark
  ( mark
  , MarkProps
  ) where

import Mantine.Core.Prelude

mark :: (MarkProps -> MarkProps) -> JSX
mark = mkTrivialComponent markComponent

foreign import markComponent :: ReactComponent MarkPropsImpl

type MarkProps =
  MantineComponent
    ( children :: String
    , color    :: Optional MantineColor
    )

type MarkPropsImpl =
  MantineComponentImpl
    ( children :: String
    , color    :: OptionalImpl MantineColorImpl
    )
