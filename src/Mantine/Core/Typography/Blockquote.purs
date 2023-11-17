module Mantine.Core.Typography.Blockquote
  ( blockquote
  , blockquote_
  , Props_Blockquote
  , Props_BlockquoteImpl
  ) where

import Mantine.Core.Prelude

blockquote
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Blockquote
  => Union attrsImpl attrsImpl_ Props_BlockquoteImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
blockquote = element (unsafeCoerce blockquoteComponent) <<< toNative

blockquote_ :: Array JSX -> JSX
blockquote_ children = blockquote { children }

foreign import blockquoteComponent :: ReactComponent (Record Props_BlockquoteImpl)

type Props_Blockquote =
  Props_Common
    ( children :: Array JSX
    , cite     :: JSX
    , color    :: MantineColor
    , icon     :: JSX
    , iconSize :: MantineNumberSize
    , radius   :: MantineNumberSize
    )

type Props_BlockquoteImpl =
  Props_CommonImpl
    ( children :: Array JSX
    , cite     :: JSX
    , color    :: MantineColorImpl
    , icon     :: JSX
    , iconSize :: MantineNumberSizeImpl
    , radius   :: MantineNumberSizeImpl
    )
