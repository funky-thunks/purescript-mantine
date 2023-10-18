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
  ThemingProps
    ( children :: Array JSX
    , color    :: Maybe DimmedOrColor
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

instance ToFFI TitleOrder Int where
  toNative = case _ of
    Title1 -> 1
    Title2 -> 2
    Title3 -> 3
    Title4 -> 4
    Title5 -> 5
    Title6 -> 6

type TitlePropsImpl =
  ThemingPropsImpl
    ( children :: Array JSX
    , color    :: Nullable String
    , order    :: Nullable Int
    , size     :: Nullable MantineNumberSizeImpl
    )
