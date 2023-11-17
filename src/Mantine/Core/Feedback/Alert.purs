module Mantine.Core.Feedback.Alert
  ( alert
  , alert_
  , Props_Alert
  , Props_AlertImpl
  , AlertVariant(..)
  , AlertVariantImpl
  ) where

import Mantine.Core.Prelude

alert
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Alert
  => Union attrsImpl attrsImpl_ Props_AlertImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
alert = element (unsafeCoerce alertComponent) <<< toNative

alert_ :: JSX -> JSX
alert_ children = alert { children }

foreign import alertComponent :: ReactComponent (Record Props_AlertImpl)

type Props_Alert =
  Props_Common
    ( children         :: JSX
    , closeButtonLabel :: String
    , color            :: MantineColor
    , icon             :: JSX
    , onClose          :: Effect Unit
    , radius           :: MantineNumberSize
    , title            :: JSX
    , variant          :: AlertVariant
    , withCloseButton  :: Boolean
    )

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

type Props_AlertImpl =
  Props_CommonImpl
    ( children         :: JSX
    , closeButtonLabel :: String
    , color            :: MantineColorImpl
    , icon             :: JSX
    , onClose          :: Effect Unit
    , radius           :: MantineNumberSizeImpl
    , title            :: JSX
    , variant          :: AlertVariantImpl
    , withCloseButton  :: Boolean
    )
