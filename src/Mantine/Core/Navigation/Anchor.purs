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
  MantineComponent
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

type AnchorUnderlineImpl = String

instance ToFFI AnchorUnderline AnchorUnderlineImpl where
  toNative = case _ of
   AnchorUnderlineAlways -> "always"
   AnchorUnderlineHover  -> "hover"
   AnchorUnderlineNever  -> "never"

type AnchorPropsImpl =
  MantineComponentImpl
    ( children  :: Array JSX
    , href      :: String
    , target    :: Nullable String
    , underline :: AnchorUnderlineImpl
    | PolymorphicImpl TextPropsImplRow
    )
