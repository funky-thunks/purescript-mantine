module Mantine.Core.Feedback.Notification
  ( notification
  , notification_
  , Props_Notification
  , Props_NotificationImpl
  ) where

import Mantine.Core.Buttons.CloseButton (Props_CloseButtonInner, Props_CloseButtonInnerImpl)
import Mantine.Core.Prelude

notification
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Notification
  => Union attrsImpl attrsImpl_ Props_NotificationImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
notification = element (unsafeCoerce notificationComponent) <<< toNative

notification_ :: Array JSX -> JSX
notification_ children = notification { children }

foreign import notificationComponent :: ReactComponent (Record Props_NotificationImpl)

type Props_Notification =
  Props_Common
    ( children         :: Array JSX
    , closeButtonProps :: Record Props_CloseButtonInner
    , color            :: MantineColor
    , icon             :: JSX
    , loading          :: Boolean
    , onClose          :: Effect Unit
    , radius           :: MantineNumberSize
    , title            :: JSX
    , withBorder       :: Boolean
    , withCloseButton  :: Boolean
    )

type Props_NotificationImpl =
  Props_CommonImpl
    ( children         :: Array JSX
    , closeButtonProps :: Record Props_CloseButtonInnerImpl
    , color            :: MantineColorImpl
    , icon             :: JSX
    , loading          :: Boolean
    , onClose          :: Effect Unit
    , radius           :: MantineNumberSizeImpl
    , title            :: JSX
    , withBorder       :: Boolean
    , withCloseButton  :: Boolean
    )
