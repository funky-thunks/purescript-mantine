module Mantine.Core.Typography.Highlight
  ( highlight
  , HighlightProps
  ) where

import Prelude
import Mantine.Core.Prelude
import Mantine.Core.Typography.Text (TextPropsRow, TextPropsImplRow)
import React.Basic.Emotion (Style)

highlight :: (HighlightProps -> HighlightProps) -> JSX
highlight = mkTrivialComponent highlightComponent

foreign import highlightComponent :: ReactComponent HighlightPropsImpl

type HighlightProps =
  ThemingProps
    ( children        :: String
    , highlight       :: Array String
    , highlightColor  :: Maybe MantineColor
    , highlightStyles :: Maybe Style
    | TextPropsRow
    )

type HighlightPropsImpl =
  ThemingPropsImpl
    ( children        :: String
    , highlight       :: Array String
    , highlightColor  :: Nullable String
    , highlightStyles :: Nullable Style
    | TextPropsImplRow
    )
