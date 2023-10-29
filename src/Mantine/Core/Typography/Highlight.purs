module Mantine.Core.Typography.Highlight
  ( highlight
  , HighlightProps
  ) where

import Mantine.Core.Prelude
import Mantine.Core.Typography.Text (TextPropsRow, TextPropsImplRow, textToImpl)
import React.Basic.DOM as DOM
import React.Basic.Emotion (Style)

highlight :: (HighlightProps -> HighlightProps) -> JSX
highlight = mkComponent highlightComponent highlightToImpl defaultThemingProps_

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
    ( children        :: Array JSX
    , highlight       :: Array String
    , highlightColor  :: Nullable String
    , highlightStyles :: Nullable Style
    | TextPropsImplRow
    )

highlightToImpl :: HighlightProps -> HighlightPropsImpl
highlightToImpl props =
  let rest = textToImpl <<< wrapChildren <<< dropLocalProps
      wrapChildren p = p { children = [ DOM.text p.children ] }
      dropLocalProps =
            delete (Proxy :: Proxy "highlight")
        <<< delete (Proxy :: Proxy "highlightColor")
        <<< delete (Proxy :: Proxy "highlightStyles")
      highlightProps =
        { highlight:       props.highlight
        , highlightColor:  props.highlightColor
        , highlightStyles: props.highlightStyles
        }
   in toNative highlightProps `union` rest props
