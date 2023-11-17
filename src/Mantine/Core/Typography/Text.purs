module Mantine.Core.Typography.Text
  ( text
  , text_
  , TextProps
  , TextPropsRow
  , TextSpecificPropsRow
  , TextTruncate(..)

  , TextPropsImplRow
  , TextTruncateImpl
  ) where

import Mantine.Core.Prelude

text :: (TextProps -> TextProps) -> JSX
text = mkTrivialComponent textComponent

text_ :: Array JSX -> JSX
text_ children = text _ { children = children }

foreign import textComponent :: ReactComponent TextPropsImpl

type TextProps = MantineComponent (TextSpecificPropsRow + TextPropsRow)

type TextSpecificPropsRow r =
  ( children :: Array JSX
  , color    :: Optional MantineColor
  , span     :: Boolean
  | r
  )

type TextPropsRow =
  ( gradient  :: Optional MantineGradient
  , inherit   :: Boolean
  , inline    :: Boolean
  , lineClamp :: Optional Int
  , size      :: Optional MantineNumberSize
  , truncate  :: Optional TextTruncate
  )

data TextTruncate
  = TextTruncate
  | TextTruncateStart
  | TextTruncateEnd

type TextTruncateImpl = Boolean |+| String

instance ToFFI TextTruncate TextTruncateImpl where
  toNative = case _ of
    TextTruncate      -> asOneOf true
    TextTruncateStart -> asOneOf "start"
    TextTruncateEnd   -> asOneOf "end"

type TextPropsImpl = MantineComponentImpl (TextSpecificPropsImplRow + TextPropsImplRow)

type TextSpecificPropsImplRow r =
  ( children :: Array JSX
  , color    :: OptionalImpl MantineColorImpl
  , span     :: Boolean
  | r
  )

type TextPropsImplRow =
  ( gradient  :: OptionalImpl MantineGradientImpl
  , inherit   :: Boolean
  , inline    :: Boolean
  , lineClamp :: OptionalImpl Number
  , size      :: OptionalImpl MantineNumberSizeImpl
  , truncate  :: OptionalImpl TextTruncateImpl
  )
