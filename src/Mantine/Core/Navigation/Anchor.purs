module Mantine.Core.Navigation.Anchor
  ( anchor
  , AnchorProps
  ) where

import Mantine.Core.Prelude
import Mantine.Core.Typography.Text (TextPropsRow, TextPropsImplRow)

anchor :: (AnchorProps -> AnchorProps) -> JSX
anchor = mkTrivialComponent anchorComponent

foreign import anchorComponent :: ReactComponent AnchorPropsImpl

type AnchorProps =
  ThemingProps
    ( children :: Array JSX
    , href     :: String
    | TextPropsRow
    )

type AnchorPropsImpl =
  ThemingPropsImpl
    ( children :: Array JSX
    , href     :: String
    | TextPropsImplRow
    )
