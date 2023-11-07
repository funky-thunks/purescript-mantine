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
  ThemingProps
    ( children :: Array JSX
    , cite     :: Maybe JSX
    , color    :: Maybe MantineColor
    , icon     :: Maybe JSX
    , iconSize :: Maybe MantineNumberSize
    , radius   :: Maybe MantineNumberSize
    )

type BlockquotePropsImpl =
  ThemingPropsImpl
    ( children :: Array JSX
    , cite     :: Nullable JSX
    , color    :: Nullable String
    , icon     :: Nullable JSX
    , iconSize :: Nullable MantineNumberSizeImpl
    , radius   :: Nullable MantineNumberSizeImpl
    )
