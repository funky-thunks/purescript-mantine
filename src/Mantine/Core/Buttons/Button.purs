module Mantine.Core.Buttons.Button
  ( button
  , button_
  , Props_Button
  , Props_ButtonImpl
  , ButtonSize(..)
  , ButtonSizeImpl
  , ButtonVariant(..)
  , ButtonVariantImpl

  , buttonGroup
  , Props_ButtonGroup
  , Props_ButtonGroupImpl

  , unstyledButton
  , Props_UnstyledButton
  , Props_UnstyledButtonImpl
  ) where

import Prelude (class Show, (<>))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Mantine.Core.Feedback.Loader (Props_Loader, Props_LoaderImpl)
import Mantine.Core.Prelude

button
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_Button
  => Union attrsImpl attrsImpl_ Props_ButtonImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
button = element (unsafeCoerce buttonComponent) <<< toNative

button_ :: JSX -> JSX
button_ child = button { children: pure child }

foreign import buttonComponent :: ReactComponent (Record Props_ButtonImpl)

type Props_Button =
  Props_Common
    ( children     :: Array JSX
    , color        :: MantineColor
    , disabled     :: Boolean
    , fullWidth    :: Boolean
    , gradient     :: MantineGradient
    , justify      :: JustifyContent
    , leftSection  :: JSX
    , loaderProps  :: Record Props_Loader
    , loading      :: Boolean
    , onClick      :: EventHandler
    , radius       :: Radius
    , rightSection :: JSX
    , size         :: ButtonSize
    , variant      :: ButtonVariant
    )

data ButtonSize
  = Padded  MantineSize
  | Compact MantineSize

type ButtonSizeImpl = MantineSizeImpl

instance ToFFI ButtonSize ButtonSizeImpl where
  toNative = case _ of
    Padded  s -> toNative s
    Compact s -> "compact-" <> toNative s

data ButtonVariant
  = ButtonVariantDefault
  | ButtonVariantFilled
  | ButtonVariantLight
  | ButtonVariantOutline
  | ButtonVariantSubtle
  | ButtonVariantTransparent
  | ButtonVariantWhite
  | ButtonVariantGradient

type ButtonVariantImpl = String

instance ToFFI ButtonVariant ButtonVariantImpl where
  toNative = case _ of
    ButtonVariantDefault     -> "default"
    ButtonVariantFilled      -> "filled"
    ButtonVariantLight       -> "light"
    ButtonVariantOutline     -> "outline"
    ButtonVariantSubtle      -> "subtle"
    ButtonVariantTransparent -> "transparent"
    ButtonVariantWhite       -> "white"
    ButtonVariantGradient    -> "gradient"

derive instance genericVariant :: Generic ButtonVariant _
instance showVariant :: Show ButtonVariant where show = genericShow

type Props_ButtonImpl =
  Props_CommonImpl
    ( children     :: Array JSX
    , color        :: MantineColorImpl
    , disabled     :: Boolean
    , fullWidth    :: Boolean
    , gradient     :: MantineGradientImpl
    , justify      :: JustifyContentImpl
    , leftSection  :: JSX
    , loaderProps  :: Record Props_LoaderImpl
    , loading      :: Boolean
    , onClick      :: EventHandler
    , radius       :: RadiusImpl
    , rightSection :: JSX
    , size         :: ButtonSizeImpl
    , variant      :: ButtonVariantImpl
    )

buttonGroup
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_ButtonGroup
  => Union attrsImpl attrsImpl_ Props_ButtonGroupImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
buttonGroup = element (unsafeCoerce buttonGroupComponent) <<< toNative

foreign import buttonGroupComponent :: ReactComponent (Record Props_ButtonGroupImpl)

type Props_ButtonGroup =
  Props_Common
    ( borderWidth :: MantineNumberSize
    , children    :: Array JSX
    , orientation :: Orientation
    )

type Props_ButtonGroupImpl =
  Props_CommonImpl
    ( borderWidth :: MantineNumberSizeImpl
    , children    :: Array JSX
    , orientation :: OrientationImpl
    )

unstyledButton
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_UnstyledButton
  => Union attrsImpl attrsImpl_ Props_UnstyledButtonImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
unstyledButton = element (unsafeCoerce unstyledButtonComponent) <<< toNative

foreign import unstyledButtonComponent :: ReactComponent (Record Props_UnstyledButtonImpl)

type Props_UnstyledButton =
  Props_Common
    ( children :: Array JSX
    , onClick  :: EventHandler
    | Polymorphic ()
    )

type Props_UnstyledButtonImpl =
  Props_CommonImpl
    ( children :: Array JSX
    , onClick  :: EventHandler
    | PolymorphicImpl ()
    )
