module Mantine.Core.Feedback.Notification
  ( notification
  , notification_
  , NotificationProps
  ) where

import Mantine.Core.Buttons.CloseButton (CloseButtonProps, CloseButtonPropsImpl)
import Mantine.Core.Prelude

notification :: (NotificationProps -> NotificationProps) -> JSX
notification = mkComponentWithDefault notificationComponent defaultNotificationProps

notification_ :: JSX -> JSX
notification_ children = notification _ { children = children }

foreign import notificationComponent :: ReactComponent NotificationPropsImpl

type NotificationProps =
  MantineComponent
    ( children         :: JSX
    , closeButtonProps :: Maybe CloseButtonProps
    , color            :: Maybe MantineColor
    , icon             :: Maybe JSX
    , loading          :: Boolean
    , onClose          :: Effect Unit
    , radius           :: Maybe MantineNumberSize
    , title            :: Maybe JSX
    , withBorder       :: Boolean
    , withCloseButton  :: Boolean
    )

defaultNotificationProps :: NotificationProps
defaultNotificationProps =
   defaultMantineComponent
     { children: mempty
     , onClose: pure unit
     }

type NotificationPropsImpl =
  MantineComponentImpl
    ( children         :: JSX
    , closeButtonProps :: Nullable CloseButtonPropsImpl
    , color            :: Nullable MantineColorImpl
    , icon             :: Nullable JSX
    , loading          :: Boolean
    , onClose          :: Effect Unit
    , radius           :: Nullable MantineNumberSizeImpl
    , title            :: Nullable JSX
    , withBorder       :: Boolean
    , withCloseButton  :: Boolean
    )
