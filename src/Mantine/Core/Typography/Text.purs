module Mantine.Core.Typography.Text
  ( text
  , text_
  , TextProps
  , TextPropsRow
  , TextPropsImplRow
  , WithChildren
  ) where

import Prelude
import Mantine.Core.Prelude

text :: (TextProps -> TextProps) -> JSX
text = mkTrivialComponent textComponent

text_ :: Array JSX -> JSX
text_ children = text _ { children = children }

foreign import textComponent :: ReactComponent TextPropsImpl

type TextProps = ThemingProps (WithChildren + TextPropsRow)

type WithChildren r =
  ( children :: Array JSX
  | r
  )

type TextPropsRow =
  ( align    :: Maybe TextAlign
  , color    :: Maybe DimmedOrColor
  , size     :: Maybe MantineNumberSize
  )

type TextPropsImpl = ThemingPropsImpl (WithChildren + TextPropsImplRow)

type TextPropsImplRow =
  ( align :: Nullable String
  , color :: Nullable String
  , size  :: Nullable MantineNumberSizeImpl
  )
