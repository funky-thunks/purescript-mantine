module Mantine.Core.Buttons.ActionIcon
  ( actionIcon
  , ActionIconProps
  , ActionIconVariant(..)
  , module Mantine.Core.Common
  ) where

import Prelude (pure, unit, (<$>))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable, notNull, null)
import Mantine.Core.Common (AlignItems(..), MantineColor(..), MantineNumberSize, MantineSize(..), Orientation(..), Position(..), Radius(..))
import Mantine.Core.Common as MC
import React.Basic (ReactComponent, element)
import React.Basic.Events (EventHandler, handler_)
import React.Basic.Hooks (JSX)
import React.Icons (icon_)
import React.Icons.Types (ReactIcon)

actionIcon :: ReactIcon -> (ActionIconProps -> ActionIconProps) -> JSX
actionIcon icon setProps = element actionIconComponent (actionIconToImpl (setProps (defaultActionIconProps icon)))

foreign import actionIconComponent :: ReactComponent ActionIconPropsImpl

type ActionIconProps =
  MC.ThemingProps
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
  MC.defaultThemingProps
    { icon
    , color: Nothing
    , disabled: false
    , loading: false
    , onClick: handler_ (pure unit)
    , size: Nothing
    , radius: Nothing
    , variant: ActionIconDefault
    }

type ActionIconPropsImpl =
  MC.ThemingPropsImpl
    ( children :: Array JSX
    , color    :: Nullable String
    , disabled :: Boolean
    , loading  :: Boolean
    , onClick  :: EventHandler
    , size     :: Nullable MC.MantineNumberSizeImpl
    , radius   :: Nullable MC.MantineNumberSizeImpl
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
actionIconToImpl =
  MC.themingToImpl \ props@{ disabled, loading, onClick } ->
    { disabled, loading, onClick
    , children: pure (icon_ props.icon)
    , color:   toNullable (MC.colorNative          <$> props.color)
    , size:    toNullable (MC.numberSizeNative     <$> props.size)
    , radius:  toNullable (MC.numberSizeNative     <$> props.radius)
    , variant: actionIconVariantNative props.variant
    }
