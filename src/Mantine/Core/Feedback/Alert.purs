module Mantine.Core.Feedback.Alert
  ( alert
  , alert_
  , AlertProps
  , AlertClosable(..)
  , AlertVariant(..)
  ) where

import Mantine.Core.Prelude

alert :: (AlertProps -> AlertProps) -> JSX
alert = mkComponent alertComponent alertToImpl defaultAlertProps

alert_ :: JSX -> JSX
alert_ children = alert _ { children = children }

foreign import alertComponent :: ReactComponent AlertPropsImpl

type AlertProps =
  MantineComponent
    ( children  :: JSX
    , closable  :: AlertClosable
    , color     :: Optional MantineColor
    , icon      :: Optional JSX
    , radius    :: Optional MantineNumberSize
    , title     :: Optional JSX
    , variant   :: Optional AlertVariant
    )

data AlertClosable
  = AlertNotClosable
  | AlertClosable String (Effect Unit)

instance DefaultValue AlertClosable where
  defaultValue = AlertNotClosable

data AlertVariant
  = AlertVariantFilled
  | AlertVariantLight
  | AlertVariantOutline
  | AlertVariantTransparent
  | AlertVariantWhite
  | AlertVariantDefault

type AlertVariantImpl = String

instance ToFFI AlertVariant AlertVariantImpl where
  toNative = case _ of
    AlertVariantFilled      -> "filled"
    AlertVariantLight       -> "light"
    AlertVariantOutline     -> "outline"
    AlertVariantTransparent -> "transparent"
    AlertVariantWhite       -> "white"
    AlertVariantDefault     -> "default"

defaultAlertProps :: AlertProps
defaultAlertProps = defaultMantineComponent { children: mempty :: JSX }

type AlertPropsImpl = MantineComponentImpl (CloseProps + AlertPropsRowImpl)

type AlertPropsRowImpl =
  ( children :: JSX
  , color    :: OptionalImpl MantineColorImpl
  , icon     :: OptionalImpl JSX
  , radius   :: OptionalImpl MantineNumberSizeImpl
  , title    :: OptionalImpl JSX
  , variant  :: OptionalImpl AlertVariantImpl
  )

type CloseProps r =
  ( closeButtonLabel :: OptionalImpl String
  , onClose          :: Effect Unit
  , withCloseButton  :: Boolean
  | r
  )

alertToImpl :: AlertProps -> AlertPropsImpl
alertToImpl props =
  let rest = toNative <<< delete (Proxy :: Proxy "closable")
   in closeProps props.closable `union` rest props

closeProps :: AlertClosable -> { | CloseProps () }
closeProps = case _ of
  AlertNotClosable ->
    { withCloseButton: false
    , closeButtonLabel: toOptionalImpl (Optional Nothing)
    , onClose: pure unit
    }
  AlertClosable message action ->
    { withCloseButton: true
    , closeButtonLabel: toOptionalImpl (pure message)
    , onClose: action
    }
