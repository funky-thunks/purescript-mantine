module Mantine.Core.Typography.Mark
  ( mark
  , MarkProps
  ) where

import Mantine.Core.Prelude

mark :: (MarkProps -> MarkProps) -> JSX
mark = mkTrivialComponent markComponent

foreign import markComponent :: ReactComponent MarkPropsImpl

type MarkProps =
  ThemingProps
    ( children :: String
    , color    :: Maybe MantineColor
    )

type MarkPropsImpl =
  ThemingPropsImpl
    ( children :: String
    , color    :: Nullable String
    )
