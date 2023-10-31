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
  , color    :: Maybe MantineColor
  , span     :: Boolean
  | r
  )

type TextPropsRow =
  ( gradient  :: Maybe MantineGradient
  , inherit   :: Boolean
  , inline    :: Boolean
  , lineClamp :: Maybe Int
  , size      :: Maybe MantineNumberSize
  , truncate  :: Maybe TextTruncate
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
  , color    :: Nullable MantineColorImpl
  , span     :: Boolean
  | r
  )

type TextPropsImplRow =
  ( gradient  :: Nullable MantineGradientImpl
  , inherit   :: Boolean
  , inline    :: Boolean
  , lineClamp :: Nullable Number
  , size      :: Nullable MantineNumberSizeImpl
  , truncate  :: Nullable TextTruncateImpl
  )
