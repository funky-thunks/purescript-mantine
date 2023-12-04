module Mantine.Core.Navigation.Anchor
  ( anchor
  , Props_Anchor
  , Props_AnchorImpl
  , AnchorUnderline(..)
  , AnchorUnderlineImpl
  ) where

import Mantine.Core.Typography.Text (Props_TextBase, Props_TextBaseImpl)
import Mantine.Core.Prelude

anchor
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Anchor
  => Union attrsImpl attrsImpl_ Props_AnchorImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
anchor = element (unsafeCoerce anchorComponent) <<< toNative

foreign import anchorComponent :: ReactComponent (Record Props_AnchorImpl)

type Props_Anchor =
  Props_Common
    ( children  :: Array JSX
    , href      :: String
    , target    :: String
    , underline :: AnchorUnderline
    | Props_TextBase
    )

data AnchorUnderline
  = AnchorUnderlineAlways
  | AnchorUnderlineHover
  | AnchorUnderlineNever

type AnchorUnderlineImpl = String

instance ToFFI AnchorUnderline AnchorUnderlineImpl where
  toNative = case _ of
   AnchorUnderlineAlways -> "always"
   AnchorUnderlineHover  -> "hover"
   AnchorUnderlineNever  -> "never"

type Props_AnchorImpl =
  Props_CommonImpl
    ( children  :: Array JSX
    , href      :: String
    , target    :: String
    , underline :: AnchorUnderlineImpl
    | Props_TextBaseImpl
    )
