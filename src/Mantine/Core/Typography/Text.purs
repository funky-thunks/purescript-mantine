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

type TextProps = ThemingProps (TextSpecificPropsRow + TextPropsRow)

type TextSpecificPropsRow r =
  ( children :: Array JSX
  , span     :: Boolean
  | r
  )

type TextPropsRow =
  ( gradient  :: Maybe MantineGradient
  , inherit   :: Boolean
  , inline    :: Boolean
  , lineClamp :: Maybe Number
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

type TextPropsImpl = ThemingPropsImpl (TextSpecificPropsRow + TextPropsImplRow)

type TextPropsImplRow =
  ( gradient  :: Nullable MantineGradientImpl
  , inherit   :: Boolean
  , inline    :: Boolean
  , lineClamp :: Nullable Number
  , size      :: Nullable MantineNumberSizeImpl
  , truncate  :: Nullable TextTruncateImpl
  )
