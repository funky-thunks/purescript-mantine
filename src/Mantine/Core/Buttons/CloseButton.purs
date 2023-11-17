module Mantine.Core.Buttons.CloseButton
  ( closeButton
  , CloseButtonProps

  , CloseButtonPropsImpl
  ) where

import Mantine.Core.Prelude

closeButton :: (CloseButtonProps -> CloseButtonProps) -> JSX
closeButton = mkTrivialComponent closeButtonComponent

foreign import closeButtonComponent :: ReactComponent CloseButtonPropsImpl

type CloseButtonProps =
  MantineComponent
    ( children :: Array JSX
    , disabled :: Boolean
    , iconSize :: Optional MantineNumberSize
    , onClick  :: Optional EventHandler
    , radius   :: Optional MantineNumberSize
    , size     :: Optional MantineNumberSize
    )

type CloseButtonPropsImpl =
  MantineComponentImpl
    ( children :: Array JSX
    , disabled :: Boolean
    , iconSize :: OptionalImpl MantineNumberSizeImpl
    , onClick  :: OptionalImpl EventHandler
    , radius   :: OptionalImpl MantineNumberSizeImpl
    , size     :: OptionalImpl MantineNumberSizeImpl
    )
