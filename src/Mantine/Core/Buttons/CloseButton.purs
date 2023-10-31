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
    , iconSize :: Maybe MantineNumberSize
    , onClick  :: Maybe EventHandler
    , radius   :: Maybe MantineNumberSize
    , size     :: Maybe MantineNumberSize
    )

type CloseButtonPropsImpl =
  MantineComponentImpl
    ( children :: Array JSX
    , disabled :: Boolean
    , iconSize :: Nullable MantineNumberSizeImpl
    , onClick  :: Nullable EventHandler
    , radius   :: Nullable MantineNumberSizeImpl
    , size     :: Nullable MantineNumberSizeImpl
    )
