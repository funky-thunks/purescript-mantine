module Mantine.Core.Buttons.CloseButton
  ( closeButton
  , Props_CloseButton
  , Props_CloseButtonImpl

  , Props_CloseButtonInner
  , Props_CloseButtonInnerImpl
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
    ( "aria-label" :: String
    , children     :: Array JSX
    , disabled     :: Boolean
    , iconSize     :: MantineNumberSize
    , onClick      :: EventHandler
    , radius       :: MantineNumberSize
    , size         :: MantineNumberSize
    )

type Props_CloseButtonImpl =
  Props_CommonImpl
    ( "aria-label" :: String
    , children     :: Array JSX
    , disabled     :: Boolean
    , iconSize     :: MantineNumberSizeImpl
    , onClick      :: EventHandler
    , radius       :: MantineNumberSizeImpl
    , size         :: MantineNumberSizeImpl
    )

type Props_CloseButtonInner =
    ( "aria-label" :: Optional String
    , disabled     :: Optional Boolean
    , iconSize     :: Optional MantineNumberSize
    , onClick      :: Optional EventHandler
    , radius       :: Optional MantineNumberSize
    , size         :: Optional MantineNumberSize
    )

type Props_CloseButtonInnerImpl =
    ( "aria-label" :: OptionalImpl String
    , disabled     :: OptionalImpl Boolean
    , iconSize     :: OptionalImpl MantineNumberSizeImpl
    , onClick      :: OptionalImpl EventHandler
    , radius       :: OptionalImpl MantineNumberSizeImpl
    , size         :: OptionalImpl MantineNumberSizeImpl
    )
