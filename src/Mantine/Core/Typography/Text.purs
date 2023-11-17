module Mantine.Core.Typography.Text
  ( text
  , text_
  , Props_Text
  , Props_TextImpl
  , Props_TextBase
  , Props_TextBaseImpl
  , Props_TextSpecific
  , Props_TextSpecificImpl
  , TextTruncate(..)
  , TextTruncateImpl
  ) where

import Mantine.Core.Prelude

text :: forall attrs attrs_ attrsImpl attrsImpl_
      . Union attrs     attrs_     Props_Text
     => Union attrsImpl attrsImpl_ Props_TextImpl
     => ToFFI (Record attrs) (Record attrsImpl)
     => Record attrs -> JSX
text = element (unsafeCoerce textComponent) <<< toNative

text_ :: Array JSX -> JSX
text_ children = text { children }

foreign import textComponent :: ReactComponent (Record Props_TextImpl)

type Props_Text = Props_Common + Props_TextSpecific + Props_TextBase

type Props_TextSpecific r =
  ( children :: Array JSX
  , color    :: MantineColor
  , span     :: Boolean
  | r
  )

type Props_TextBase =
  ( gradient  :: MantineGradient
  , inherit   :: Boolean
  , inline    :: Boolean
  , lineClamp :: Int
  , size      :: MantineNumberSize
  , truncate  :: TextTruncate
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

type Props_TextImpl = Props_CommonImpl + Props_TextSpecificImpl + Props_TextBaseImpl

type Props_TextSpecificImpl r =
  ( children :: Array JSX
  , color    :: MantineColorImpl
  , span     :: Boolean
  | r
  )

type Props_TextBaseImpl =
  ( gradient  :: MantineGradientImpl
  , inherit   :: Boolean
  , inline    :: Boolean
  , lineClamp :: Number
  , size      :: MantineNumberSizeImpl
  , truncate  :: TextTruncateImpl
  )
