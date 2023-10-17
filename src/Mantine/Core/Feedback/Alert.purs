module Mantine.Core.Feedback.Alert
  ( alert
  , alert_
  , AlertProps
  , AlertClosable(..)
  , AlertVariant(..)

  , module Mantine.Core.Common
  ) where

import Prelude
import Data.Default (defaultValue)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, null, toNullable)
import Effect (Effect)
import Mantine.Core.Common (MantineColor(..), MantineNumberSize, MantineSize(..))
import Mantine.Core.Common as MC
import Mantine.FFI (class ToFFI, toNative)
import React.Basic (ReactComponent, element)
import React.Basic.Hooks (JSX)
import Record (delete, union)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

alert :: (AlertProps -> AlertProps) -> JSX
alert setProps = element alertComponent (alertToImpl (setProps defaultAlertProps))

alert_ :: JSX -> JSX
alert_ children = alert _ { children = children }

foreign import alertComponent :: ReactComponent AlertPropsImpl

type AlertProps =
  MC.ThemingProps
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

data AlertVariant
  = AlertVariantOutline
  | AlertVariantLight
  | AlertVariantFilled

instance ToFFI AlertVariant String where
  toNative = case _ of
    AlertVariantOutline -> "outline"
    AlertVariantLight   -> "light"
    AlertVariantFilled  -> "filled"

defaultAlertProps :: AlertProps
defaultAlertProps =
  MC.defaultThemingProps
    { children: mempty :: JSX
    , closable: NotClosable
    , variant:  AlertVariantLight
    } `union` defaultValue

type AlertPropsImpl = MC.ThemingPropsImpl (CloseProps + AlertPropsRowImpl)

type AlertPropsRowImpl =
  ( children :: JSX
  , color    :: Nullable String
  , icon     :: Nullable JSX
  , radius   :: Nullable MC.MantineNumberSizeImpl
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
  closeProps props.closable `union` toNative (delete (Proxy :: Proxy "closable") props)

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
