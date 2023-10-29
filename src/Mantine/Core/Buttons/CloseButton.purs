module Mantine.Core.Buttons.CloseButton
  ( closeButton
  , CloseButtonProps

  , CloseButtonPropsImpl
  , closeButtonPropsToImpl
  ) where

import Mantine.Core.Buttons.ActionIcon (ActionIconPropsRow, ActionIconPropsImplRow, actionIconToImpl, defaultActionIconProps)
import Mantine.Core.Prelude
import React.Icons.Types (ReactIcon)

closeButton :: ReactIcon -> (CloseButtonProps -> CloseButtonProps) -> JSX
closeButton = mkComponent closeButtonComponent closeButtonPropsToImpl <<< defaultCloseButtonProps

foreign import closeButtonComponent :: ReactComponent CloseButtonPropsImpl

type CloseButtonProps =
  ThemingProps
    ( iconSize :: Maybe MantineNumberSize
    | ActionIconPropsRow
    )

defaultCloseButtonProps :: ReactIcon -> CloseButtonProps
defaultCloseButtonProps icon = defaultActionIconProps icon `union` defaultValue

type CloseButtonPropsImpl =
  ThemingPropsImpl
    ( iconSize :: Nullable MantineNumberSizeImpl
    | ActionIconPropsImplRow
    )

closeButtonPropsToImpl :: CloseButtonProps -> CloseButtonPropsImpl
closeButtonPropsToImpl props = actionIconToImpl (delete (Proxy :: Proxy "iconSize") props)
  `union` toNative { iconSize: props.iconSize }
