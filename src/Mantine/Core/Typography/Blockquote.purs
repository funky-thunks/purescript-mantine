module Mantine.Core.Typography.Blockquote
  ( blockquote
  , blockquote_
  , BlockquoteProps
  ) where

import Mantine.Core.Prelude

blockquote :: (BlockquoteProps -> BlockquoteProps) -> JSX
blockquote = mkTrivialComponent blockquoteComponent

blockquote_ :: Array JSX -> JSX
blockquote_ children = blockquote _ { children = children }

foreign import blockquoteComponent :: ReactComponent BlockquotePropsImpl

type BlockquoteProps =
  MantineComponent
    ( children :: Array JSX
    , cite     :: Optional JSX
    , color    :: Optional MantineColor
    , icon     :: Optional JSX
    , iconSize :: Optional MantineNumberSize
    , radius   :: Optional MantineNumberSize
    )

type BlockquotePropsImpl =
  MantineComponentImpl
    ( children :: Array JSX
    , cite     :: OptionalImpl JSX
    , color    :: OptionalImpl MantineColorImpl
    , icon     :: OptionalImpl JSX
    , iconSize :: OptionalImpl MantineNumberSizeImpl
    , radius   :: OptionalImpl MantineNumberSizeImpl
    )
