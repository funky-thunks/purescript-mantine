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
    , closeButtonProps :: Optional CloseButtonProps
    , color            :: Optional MantineColor
    , icon             :: Optional JSX
    , loading          :: Boolean
    , onClose          :: Effect Unit
    , radius           :: Optional MantineNumberSize
    , title            :: Optional JSX
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
    , closeButtonProps :: OptionalImpl CloseButtonPropsImpl
    , color            :: OptionalImpl MantineColorImpl
    , icon             :: OptionalImpl JSX
    , loading          :: Boolean
    , onClose          :: Effect Unit
    , radius           :: OptionalImpl MantineNumberSizeImpl
    , title            :: OptionalImpl JSX
    , withBorder       :: Boolean
    , withCloseButton  :: Boolean
    )
