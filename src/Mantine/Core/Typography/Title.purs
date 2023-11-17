module Mantine.Core.Typography.Title
  ( title
  , title_
  , title1
  , title2
  , title3
  , title4
  , title5
  , title6
  , TitleProps
  , TitleOrder(..)
  ) where

import Mantine.Core.Prelude
import React.Basic.DOM as DOM

title :: (TitleProps -> TitleProps) -> JSX
title = mkTrivialComponent titleComponent

title_ :: TitleOrder -> String -> JSX
title_ order t = title _ { order = pure order, children = [ DOM.text t ] }

title1 :: String -> JSX
title1  = title_ Title1

title2 :: String -> JSX
title2  = title_ Title2

title3 :: String -> JSX
title3  = title_ Title3

title4 :: String -> JSX
title4  = title_ Title4

title5 :: String -> JSX
title5  = title_ Title5

title6 :: String -> JSX
title6  = title_ Title6

foreign import titleComponent :: ReactComponent TitlePropsImpl

type TitleProps =
  MantineComponent
    ( children :: Array JSX
    , order    :: Optional TitleOrder
    , size     :: Optional MantineNumberSize
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
    , order    :: OptionalImpl TitleOrderImpl
    , size     :: OptionalImpl MantineNumberSizeImpl
    )
