module Mantine.Core.Navigation.Anchor
  ( anchor
  , AnchorProps
  , AnchorUnderline(..)
  ) where

import Mantine.Core.Typography.Text (TextPropsRow, TextPropsImplRow)
import Mantine.Core.Prelude

anchor :: (AnchorProps -> AnchorProps) -> JSX
anchor = mkTrivialComponent anchorComponent

foreign import anchorComponent :: ReactComponent AnchorPropsImpl

type AnchorProps =
  ThemingProps
    ( children  :: Array JSX
    , href      :: String
    , target    :: Maybe String
    , underline :: AnchorUnderline
    | Polymorphic TextPropsRow
    )

data AnchorUnderline
  = AnchorUnderlineAlways
  | AnchorUnderlineHover
  | AnchorUnderlineNever

instance DefaultValue AnchorUnderline where
  defaultValue = AnchorUnderlineHover

instance ToFFI AnchorUnderline String where
  toNative = case _ of
   AnchorUnderlineAlways -> "always"
   AnchorUnderlineHover  -> "hover"
   AnchorUnderlineNever  -> "never"

type AnchorPropsImpl =
  ThemingPropsImpl
    ( children  :: Array JSX
    , href      :: String
    , target    :: Nullable String
    , underline :: String
    | PolymorphicImpl TextPropsImplRow
    )
