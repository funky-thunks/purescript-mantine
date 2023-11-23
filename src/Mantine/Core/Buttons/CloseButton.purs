module Mantine.Core.Buttons.CloseButton
  ( closeButton
  , Props_CloseButton

  , Props_CloseButtonImpl
  ) where

import Mantine.Core.Prelude

closeButton
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_CloseButton
  => Union attrsImpl attrsImpl_ Props_CloseButtonImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
closeButton = element (unsafeCoerce closeButtonComponent) <<< toNative

foreign import closeButtonComponent :: ReactComponent (Record Props_CloseButtonImpl)

type Props_CloseButton =
  Props_Common
    ( children :: Array JSX
    , disabled :: Boolean
    , iconSize :: MantineNumberSize
    , onClick  :: EventHandler
    , radius   :: MantineNumberSize
    , size     :: MantineNumberSize
    )

type Props_CloseButtonImpl =
  Props_CommonImpl
    ( children :: Array JSX
    , disabled :: Boolean
    , iconSize :: MantineNumberSizeImpl
    , onClick  :: EventHandler
    , radius   :: MantineNumberSizeImpl
    , size     :: MantineNumberSizeImpl
    )
