module Mantine.Core.Typography.Title
  ( title
  , title_
  , TitleProps
  , TitleOrder(..)
  ) where

import Mantine.Core.Prelude

title :: (TitleProps -> TitleProps) -> JSX
title = mkTrivialComponent titleComponent

title_ :: Array JSX -> JSX
title_ children = title _ { children = children }

foreign import titleComponent :: ReactComponent TitlePropsImpl

type TitleProps =
  MantineComponent
    ( children :: Array JSX
    , order    :: Maybe TitleOrder
    , size     :: Maybe MantineNumberSize
    )

data TitleOrder
  = Title1
  | Title2
  | Title3
  | Title4
  | Title5
  | Title6

type TitleOrderImpl = Int

instance ToFFI TitleOrder TitleOrderImpl where
  toNative = case _ of
    Title1 -> 1
    Title2 -> 2
    Title3 -> 3
    Title4 -> 4
    Title5 -> 5
    Title6 -> 6

type TitlePropsImpl =
  MantineComponentImpl
    ( children :: Array JSX
    , order    :: Nullable TitleOrderImpl
    , size     :: Nullable MantineNumberSizeImpl
    )
