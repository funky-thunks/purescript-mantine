module Mantine.Core.Buttons.ActionIcon
  ( actionIcon
  , ActionIconProps
  , ActionIconVariant(..)

  , actionIconGroup
  , actionIconGroup_
  , ActionIconGroupOrientation(..)
  , ActionIconGroupProps

  , defaultActionIconProps
  , actionIconToImpl
  , ActionIconPropsRow
  , ActionIconPropsImplRow
  , ActionIconVariantImpl
  ) where

import Mantine.Core.Feedback.Loader (LoaderProps, LoaderPropsImpl)
import Mantine.Core.Prelude
import React.Icons (icon_)
import React.Icons.Types (ReactIcon)

actionIcon :: ReactIcon -> (ActionIconProps -> ActionIconProps) -> JSX
actionIcon = mkComponent actionIconComponent actionIconToImpl <<< defaultActionIconProps

foreign import actionIconComponent :: ReactComponent ActionIconPropsImpl

type ActionIconProps = MantineComponent ActionIconPropsRow

type ActionIconPropsRow =
  ( color       :: Maybe MantineColor
  , disabled    :: Boolean
  , icon        :: ReactIcon
  , loading     :: Boolean
  , loaderProps :: Maybe LoaderProps
  , onClick     :: Maybe EventHandler
  , radius      :: Maybe MantineNumberSize
  , size        :: Maybe MantineNumberSize
  , variant     :: ActionIconVariant
  )

defaultActionIconProps :: ReactIcon -> ActionIconProps
defaultActionIconProps icon = defaultMantineComponent { icon }

type ActionIconPropsImpl = MantineComponentImpl ActionIconPropsImplRow

type ActionIconPropsImplRow =
  ( children    :: Array JSX
  , color       :: Nullable MantineColorImpl
  , disabled    :: Boolean
  , gradient    :: Nullable MantineGradientImpl
  , loaderProps :: Nullable LoaderPropsImpl
  , loading     :: Boolean
  , onClick     :: Nullable EventHandler
  , radius      :: Nullable MantineNumberSizeImpl
  , size        :: Nullable MantineNumberSizeImpl
  , variant     :: ActionIconVariantImpl
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

type ActionIconVariantImpl = Nullable String

instance ToFFI ActionIconVariant ActionIconVariantImpl where
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

actionIconGroup :: (ActionIconGroupProps -> ActionIconGroupProps) -> JSX
actionIconGroup = mkTrivialComponent actionIconGroupComponent

actionIconGroup_ :: Array JSX -> JSX
actionIconGroup_ children = actionIconGroup _ { children = children }

foreign import actionIconGroupComponent :: ReactComponent ActionIconGroupPropsImpl

type ActionIconGroupProps =
  MantineComponent
    ( borderWidth :: Maybe MantineNumberSize
    , children    :: Array JSX
    , orientation :: ActionIconGroupOrientation
    )

data ActionIconGroupOrientation
  = ActionIconGroupOrientationHorizontal
  | ActionIconGroupOrientationVertical

instance DefaultValue ActionIconGroupOrientation where
  defaultValue = ActionIconGroupOrientationHorizontal

type ActionIconGroupOrientationImpl = String

instance ToFFI ActionIconGroupOrientation ActionIconGroupOrientationImpl where
  toNative = case _ of
    ActionIconGroupOrientationHorizontal -> "horizontal"
    ActionIconGroupOrientationVertical   -> "vertical"

type ActionIconGroupPropsImpl =
  MantineComponentImpl
    ( borderWidth :: Nullable MantineNumberSizeImpl
    , children    :: Array JSX
    , orientation :: ActionIconGroupOrientationImpl
    )
