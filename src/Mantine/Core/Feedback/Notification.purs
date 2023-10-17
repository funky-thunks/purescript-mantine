module Mantine.Core.Feedback.Notification
  ( notification
  , notification_
  , NotificationProps

  , module Mantine.Core.Common
  ) where

import Prelude
import Data.Default (defaultValue)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Effect (Effect)
import Mantine.Core.Common (Dimension, MantineColor(..), MantineNumberSize, MantineSize(..))
import Mantine.Core.Common as MC
import Mantine.FFI (toNative)
import React.Basic (ReactComponent, element)
import React.Basic.Hooks (JSX)
import Record (union)

notification :: (NotificationProps -> NotificationProps) -> JSX
notification setProps = element notificationComponent (toNative (setProps defaultNotificationProps))

notification_ :: JSX -> JSX
notification_ children = notification _ { children = children }

foreign import notificationComponent :: ReactComponent NotificationPropsImpl

type NotificationProps =
  MC.ThemingProps
    ( children      :: JSX
    , color         :: Maybe MantineColor
    , disallowClose :: Boolean
    , icon          :: Maybe JSX
    , loading       :: Boolean
    , onClose       :: Effect Unit
    , radius        :: Maybe MantineNumberSize
    , title         :: Maybe JSX
    )

defaultNotificationProps :: NotificationProps
defaultNotificationProps =
   MC.defaultThemingProps
     { children: mempty
     , onClose: pure unit
     } `union` defaultValue

type NotificationPropsImpl =
  MC.ThemingPropsImpl
    ( children      :: JSX
    , color         :: Nullable String
    , disallowClose :: Boolean
    , icon          :: Nullable JSX
    , loading       :: Boolean
    , onClose       :: Effect Unit
    , radius        :: Nullable MC.MantineNumberSizeImpl
    , title         :: Nullable JSX
    )
