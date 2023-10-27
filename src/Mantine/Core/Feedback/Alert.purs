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
  ThemingProps
    ( children  :: JSX
    , color     :: Maybe MantineColor
    , icon      :: Maybe JSX
    , radius    :: Maybe MantineNumberSize
    , title     :: Maybe JSX
    , variant   :: AlertVariant
    , closable  :: AlertClosable
    )

data AlertClosable
  = NotClosable
  | Closable String (Effect Unit)

instance DefaultValue AlertClosable where defaultValue = NotClosable

data AlertVariant
  = AlertVariantOutline
  | AlertVariantLight
  | AlertVariantFilled

instance DefaultValue AlertVariant where defaultValue = AlertVariantLight

instance ToFFI AlertVariant String where
  toNative = case _ of
    AlertVariantOutline -> "outline"
    AlertVariantLight   -> "light"
    AlertVariantFilled  -> "filled"

defaultAlertProps :: AlertProps
defaultAlertProps =
  defaultThemingProps
    { children: mempty :: JSX
    }

type AlertPropsImpl = ThemingPropsImpl (CloseProps + AlertPropsRowImpl)

type AlertPropsRowImpl =
  ( children :: JSX
  , color    :: Nullable String
  , icon     :: Nullable JSX
  , radius   :: Nullable MantineNumberSizeImpl
  , title    :: Nullable JSX
  , variant  :: String
  )

type CloseProps r =
  ( closeButtonLabel :: Nullable String
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
  NotClosable ->
    { withCloseButton: false
    , closeButtonLabel: null
    , onClose: pure unit
    }
  Closable message action ->
    { withCloseButton: true
    , closeButtonLabel: toNullable (pure message)
    , onClose: action
    }
