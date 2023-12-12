module Mantine.Core.Typography.Title
  ( title
  , title_
  , title1
  , title2
  , title3
  , title4
  , title5
  , title6
  , Props_Title
  , Props_TitleImpl
  , TitleOrder(..)
  , TitleOrderImpl
  ) where

import Mantine.Core.Prelude
import React.Basic.DOM as DOM

title :: forall attrs attrs_ attrsImpl attrsImpl_
       . Union attrs     attrs_     Props_Title
      => Union attrsImpl attrsImpl_ Props_TitleImpl
      => ToFFI (Record attrs) (Record attrsImpl)
      => Record attrs -> JSX
title = element (unsafeCoerce titleComponent) <<< toNative

title_ :: TitleOrder -> String -> JSX
title_ order t = title { order, children: [ DOM.text t ] }

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

foreign import titleComponent :: ReactComponent (Record Props_TitleImpl)

type Props_Title =
  Props_Common
    ( children  :: Array JSX
    , lineClamp :: Number
    , order     :: TitleOrder
    , size      :: MantineNumberSize
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

type Props_TitleImpl =
  Props_CommonImpl
    ( children  :: Array JSX
    , lineClamp :: Number
    , order     :: TitleOrderImpl
    , size      :: MantineNumberSizeImpl
    )
