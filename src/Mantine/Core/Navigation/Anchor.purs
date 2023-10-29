module Mantine.Core.Navigation.Anchor
  ( anchor
  , AnchorProps
  ) where

import Mantine.Core.Prelude
import Mantine.Core.Typography.Text (TextPropsRow, TextPropsImplRow, textToImpl)

anchor :: (AnchorProps -> AnchorProps) -> JSX
anchor = mkComponent anchorComponent anchorToImpl defaultThemingProps_

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

anchorToImpl :: AnchorProps -> AnchorPropsImpl
anchorToImpl props =
  let rest = textToImpl <<< dropLocalProps
      dropLocalProps = delete (Proxy :: Proxy "href")
      anchorProps = { href: props.href }
   in anchorProps `union` rest props
