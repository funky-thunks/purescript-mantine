module Mantine.Core.Typography.Highlight
  ( highlight
  , highlight_
  , Props_Highlight
  , Props_HighlightImpl
  ) where

import Mantine.Core.Prelude
import Mantine.Core.Typography.Text (Props_TextBase, Props_TextBaseImpl)
import React.Basic.DOM as DOM
import React.Basic.Emotion (Style)

highlight
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Highlight
  => Union attrsImpl attrsImpl_ Props_HighlightImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
highlight = element (unsafeCoerce highlightComponent) <<< toNative


highlight_ :: String -> JSX
highlight_ t = highlight { children: [ DOM.text t ] }

foreign import highlightComponent :: ReactComponent (Record Props_HighlightImpl)

type Props_Highlight =
  Props_Common
    ( children        :: Array JSX
    , color           :: MantineColor
    , highlight       :: Array String
    , highlightStyles :: Style
    , span            :: Boolean
    | Props_TextBase
    )

type Props_HighlightImpl =
  Props_CommonImpl
    ( children        :: Array JSX
    , color           :: MantineColorImpl
    , highlight       :: Array String
    , highlightStyles :: Style
    , span            :: Boolean
    | Props_TextBaseImpl
    )
