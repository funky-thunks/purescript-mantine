module Mantine.Core.Typography.Highlight
  ( highlight
  , HighlightProps
  ) where

import Mantine.Core.Prelude
import Mantine.Core.Typography.Text (TextPropsRow, TextPropsImplRow)
import React.Basic.DOM as DOM
import React.Basic.Emotion (Style)

highlight :: (HighlightProps -> HighlightProps) -> JSX
highlight = mkComponent highlightComponent highlightToImpl defaultMantineComponent_

foreign import highlightComponent :: ReactComponent HighlightPropsImpl

type HighlightProps =
  MantineComponent
    ( children        :: String
    , color           :: Maybe MantineColor
    , highlight       :: Array String
    , highlightStyles :: Maybe Style
    , span            :: Boolean
    | TextPropsRow
    )

type HighlightPropsImpl =
  MantineComponentImpl
    ( children        :: Array JSX
    , color           :: Nullable MantineColorImpl
    , highlight       :: Array String
    , highlightStyles :: Nullable Style
    , span            :: Boolean
    | TextPropsImplRow
    )

highlightToImpl :: HighlightProps -> HighlightPropsImpl
highlightToImpl props =
  let rest = toNative <<< wrapChildren <<< dropLocalProps
      wrapChildren p = p { children = [ DOM.text p.children ] }
      dropLocalProps =
            delete (Proxy :: Proxy "color")
        <<< delete (Proxy :: Proxy "highlight")
        <<< delete (Proxy :: Proxy "highlightStyles")
      highlightProps =
        { color:           props.color
        , highlight:       props.highlight
        , highlightStyles: props.highlightStyles
        }
   in toNative highlightProps `union` rest props
