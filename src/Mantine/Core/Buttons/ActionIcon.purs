module Mantine.Core.Buttons.ActionIcon
  ( actionIcon
  , ActionIconProps
  , ActionIconVariant(..)

  , defaultActionIconProps
  , actionIconToImpl
  , ActionIconPropsRow
  , ActionIconPropsImplRow
  ) where

import Mantine.Core.Prelude
import React.Icons (icon_)
import React.Icons.Types (ReactIcon)

actionIcon :: ReactIcon -> (ActionIconProps -> ActionIconProps) -> JSX
actionIcon = mkComponent actionIconComponent actionIconToImpl <<< defaultActionIconProps

foreign import actionIconComponent :: ReactComponent ActionIconPropsImpl

type ActionIconProps = ThemingProps ActionIconPropsRow

type ActionIconPropsRow =
  ( color    :: Maybe MantineColor
  , disabled :: Boolean
  , icon     :: ReactIcon
  , loading  :: Boolean
  , onClick  :: EventHandler
  , radius   :: Maybe MantineNumberSize
  , size     :: Maybe MantineNumberSize
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
  , gradient :: Nullable MantineGradientImpl
  , loading  :: Boolean
  , onClick  :: EventHandler
  , radius   :: Nullable MantineNumberSizeImpl
  , size     :: Nullable MantineNumberSizeImpl
  , variant  :: Nullable String
  )

data ActionIconVariant
  = ActionIconOutline
  | ActionIconTransparent
  | ActionIconLight
  | ActionIconDefault
  | ActionIconFilled
  | ActionIconSubtle
  | ActionIconGradient MantineGradient

instance DefaultValue ActionIconVariant where defaultValue = ActionIconDefault

instance ToFFI ActionIconVariant (Nullable String) where
  toNative = case _ of
    ActionIconOutline     -> notNull "outline"
    ActionIconTransparent -> notNull "transparent"
    ActionIconLight       -> notNull "light"
    ActionIconDefault     -> null
    ActionIconFilled      -> notNull "filled"
    ActionIconSubtle      -> notNull "subtle"
    ActionIconGradient _  -> notNull "gradient"

getGradient :: ActionIconVariant -> Maybe MantineGradient
getGradient = case _ of
  ActionIconGradient g -> Just g
  _                    -> Nothing

actionIconToImpl :: ActionIconProps -> ActionIconPropsImpl
actionIconToImpl props =
  let rest = toNative <<< delete (Proxy :: Proxy "icon")
      gradient = toNative (getGradient props.variant)
   in { children: pure (icon_ props.icon), gradient } `union` rest props
