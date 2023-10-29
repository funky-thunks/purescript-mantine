module Mantine.Core.Typography.Text
  ( text
  , text_
  , TextProps
  , TextPropsRow
  , WithChildren
  , TextTransform(..)
  , TextTruncate(..)
  , TextVariant(..)

  , TextPropsImplRow
  , TruncateImpl
  , textToImpl
  ) where

import Mantine.Core.Prelude

text :: (TextProps -> TextProps) -> JSX
text = mkComponent textComponent textToImpl defaultThemingProps_

text_ :: Array JSX -> JSX
text_ children = text _ { children = children }

foreign import textComponent :: ReactComponent TextPropsImpl

type TextProps = ThemingProps (WithChildren + TextPropsRow)

type WithChildren r =
  ( children :: Array JSX
  | r
  )

type TextPropsRow =
  ( align         :: Maybe TextAlign
  , color         :: Maybe DimmedOrColor
  , inherit       :: Boolean
  , inline        :: Boolean
  , italic        :: Boolean
  , lineClamp     :: Maybe Number
  , size          :: Maybe MantineNumberSize
  , span          :: Boolean
  , strikethrough :: Boolean
  , transform     :: Maybe TextTransform
  , truncate      :: Maybe TextTruncate
  , underline     :: Boolean
  , variant       :: TextVariant
  , weight        :: Maybe FontWeight
  )

data TextTransform
  = TextTransformInherit
  | TextTransformInitial
  | TextTransformRevert
  | TextTransformUnset
  | TextTransformNone
  | TextTransformCapitalize
  | TextTransformFullSizeKana
  | TextTransformFullWidth
  | TextTransformLowercase
  | TextTransformUppercase
  | TextTransformMozInitial

instance ToFFI TextTransform String where
  toNative = case _ of
    TextTransformInherit      -> "inherit"
    TextTransformInitial      -> "initial"
    TextTransformRevert       -> "revert"
    TextTransformUnset        -> "unset"
    TextTransformNone         -> "none"
    TextTransformCapitalize   -> "capitalize"
    TextTransformFullSizeKana -> "full-size-kana"
    TextTransformFullWidth    -> "full-width"
    TextTransformLowercase    -> "lowercase"
    TextTransformUppercase    -> "uppercase"
    TextTransformMozInitial   -> "-moz-initial"

data TextTruncate = TextTruncate | TextTruncateEnd | TextTruncateStart

type TruncateImpl = Boolean |+| String

data TextVariant
  = TextVariantText
  | TextVariantGradient MantineGradient

instance DefaultValue TextVariant where defaultValue = TextVariantText

instance ToFFI TextVariant String where
  toNative = case _ of
    TextVariantText       -> "text"
    TextVariantGradient _ -> "gradient"

instance ToFFI TextTruncate TruncateImpl where
  toNative = case _ of
    TextTruncate      -> asOneOf true
    TextTruncateEnd   -> asOneOf "end"
    TextTruncateStart -> asOneOf "start"

type TextPropsImpl = ThemingPropsImpl (WithChildren + TextPropsImplRow)

type TextPropsImplRow =
  ( align         :: Nullable String
  , color         :: Nullable String
  , gradient      :: Nullable MantineGradientImpl
  , inherit       :: Boolean
  , inline        :: Boolean
  , italic        :: Boolean
  , lineClamp     :: Nullable Number
  , size          :: Nullable MantineNumberSizeImpl
  , span          :: Boolean
  , strikethrough :: Boolean
  , transform     :: Nullable String
  , truncate      :: Nullable TruncateImpl
  , underline     :: Boolean
  , variant       :: String
  , weight        :: Nullable Number
  )

textToImpl :: TextProps -> TextPropsImpl
textToImpl props =
  let gradient = case props.variant of
        TextVariantGradient g -> pure g
        _                     -> Nothing
   in toNative ({ gradient } `union` props)
