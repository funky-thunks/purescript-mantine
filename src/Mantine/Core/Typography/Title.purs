module Mantine.Core.Typography.Title
  ( title
  , title_
  , TitleProps
  , TitleOrder(..)
  ) where

import Mantine.Core.Prelude
import Mantine.Core.Typography.Text (TextPropsRow, TextPropsImplRow, textToImpl)

title :: (TitleProps -> TitleProps) -> JSX
title = mkComponent titleComponent titleToImpl defaultThemingProps_

title_ :: Array JSX -> JSX
title_ children = title _ { children = children }

foreign import titleComponent :: ReactComponent TitlePropsImpl

type TitleProps =
  ThemingProps
    ( children :: Array JSX
    , order    :: Maybe TitleOrder
    | TextPropsRow
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
    , order    :: Nullable Int
    | TextPropsImplRow
    )

titleToImpl :: TitleProps -> TitlePropsImpl
titleToImpl props =
  let rest = textToImpl <<< dropLocalProps
      dropLocalProps = delete (Proxy :: Proxy "order")
   in toNative { order: props.order } `union` rest props
