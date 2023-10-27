module Mantine.Core.Buttons.ActionIcon
  ( actionIcon
  , ActionIconProps
  , ActionIconVariant(..)

  , defaultActionIconProps
  , actionIconToImpl
  , ActionIconPropsRow
  , ActionIconPropsImplRow
  ) where

import Prelude (pure, unit)
import Mantine.Core.Prelude
import React.Basic.Events (EventHandler, handler_)
import React.Icons (icon_)
import React.Icons.Types (ReactIcon)

actionIcon :: ReactIcon -> (ActionIconProps -> ActionIconProps) -> JSX
actionIcon = mkComponent actionIconComponent actionIconToImpl <<< defaultActionIconProps

foreign import actionIconComponent :: ReactComponent ActionIconPropsImpl

type ActionIconProps = ThemingProps ActionIconPropsRow

type ActionIconPropsRow =
  ( icon     :: ReactIcon
  , color    :: Maybe MantineColor
  , disabled :: Boolean
  , loading  :: Boolean
  , onClick  :: EventHandler
  , size     :: Maybe MantineNumberSize
  , radius   :: Maybe MantineNumberSize
  , variant  :: ActionIconVariant
  )

defaultActionIconProps :: ReactIcon -> ActionIconProps
defaultActionIconProps icon =
  defaultThemingProps
    { icon
    , onClick: handler_ (pure unit)
    }

type ActionIconPropsImpl = ThemingPropsImpl ActionIconPropsImplRow

type ActionIconPropsImplRow =
  ( children :: Array JSX
  , color    :: Nullable String
  , disabled :: Boolean
  , loading  :: Boolean
  , onClick  :: EventHandler
  , size     :: Nullable MantineNumberSizeImpl
  , radius   :: Nullable MantineNumberSizeImpl
  , variant  :: Nullable String
  )

data ActionIconVariant
  = ActionIconOutline
  | ActionIconTransparent
  | ActionIconLight
  | ActionIconDefault
  | ActionIconFilled
  | ActionIconSubtle
  | ActionIconGradient

instance DefaultValue ActionIconVariant where defaultValue = ActionIconDefault

instance ToFFI ActionIconVariant (Nullable String) where toNative = actionIconVariantNative

actionIconVariantNative :: ActionIconVariant -> Nullable String
actionIconVariantNative = case _ of
  ActionIconOutline     -> notNull "outline"
  ActionIconTransparent -> notNull "transparent"
  ActionIconLight       -> notNull "light"
  ActionIconDefault     -> null
  ActionIconFilled      -> notNull "filled"
  ActionIconSubtle      -> notNull "subtle"
  ActionIconGradient    -> notNull "gradient"

actionIconToImpl :: ActionIconProps -> ActionIconPropsImpl
actionIconToImpl props =
  toNative (delete (Proxy :: Proxy "icon") props)
    `union` { children: pure (icon_ props.icon) }
