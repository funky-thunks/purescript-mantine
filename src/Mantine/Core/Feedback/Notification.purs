module Mantine.Core.Feedback.Notification
  ( notification
  , notification_
  , NotificationProps
  ) where

import Prelude
import Mantine.Core.Prelude

notification :: (NotificationProps -> NotificationProps) -> JSX
notification = mkComponentWithDefault notificationComponent defaultNotificationProps

notification_ :: JSX -> JSX
notification_ children = notification _ { children = children }

foreign import notificationComponent :: ReactComponent NotificationPropsImpl

type NotificationProps =
  ThemingProps
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
   defaultThemingProps
     { children: mempty
     , onClose: pure unit
     } `union` defaultValue

type NotificationPropsImpl =
  ThemingPropsImpl
    ( children      :: JSX
    , color         :: Nullable String
    , disallowClose :: Boolean
    , icon          :: Nullable JSX
    , loading       :: Boolean
    , onClose       :: Effect Unit
    , radius        :: Nullable MantineNumberSizeImpl
    , title         :: Nullable JSX
    )
