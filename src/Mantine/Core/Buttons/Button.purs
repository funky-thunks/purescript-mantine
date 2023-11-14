module Mantine.Core.Buttons.Button
  ( button
  , button_
  , ButtonProps
  , ButtonSize(..)
  , ButtonVariant(..)
  , LoaderPosition(..)

  , buttonGroup
  , ButtonGroupProps

  , unstyledButton
  , UnstyledButtonProps
  ) where

import Prelude (class Show, (<>))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Mantine.Core.Feedback.Loader (LoaderProps, LoaderPropsImpl)
import Mantine.Core.Prelude
import React.Basic.Events (EventHandler, handler_)

button :: (ButtonProps -> ButtonProps) -> JSX
button = mkComponent buttonComponent buttonToImpl defaultButtonProps

button_ :: JSX -> JSX
button_ child = button _ { children = pure child }

foreign import buttonComponent :: ReactComponent ButtonPropsImpl

type ButtonProps =
  MantineComponent
    ( children     :: Array JSX
    , color        :: Maybe MantineColor
    , disabled     :: Boolean
    , fullWidth    :: Boolean
    , justify      :: Maybe JustifyContent
    , leftSection  :: Maybe JSX
    , loaderProps  :: Maybe LoaderProps
    , loading      :: Boolean
    , onClick      :: EventHandler
    , radius       :: Maybe Radius
    , rightSection :: Maybe JSX
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

data LoaderPosition
  = LoaderPositionLeft
  | LoaderPositionRight
  | LoaderPositionCenter

instance ToFFI LoaderPosition String where
  toNative = case _ of
    LoaderPositionLeft   -> "left"
    LoaderPositionRight  -> "right"
    LoaderPositionCenter -> "center"

data ButtonVariant
  = ButtonVariantDefault
  | ButtonVariantFilled
  | ButtonVariantLight
  | ButtonVariantOutline
  | ButtonVariantSubtle
  | ButtonVariantTransparent
  | ButtonVariantWhite
  | ButtonVariantGradient MantineGradient

instance DefaultValue ButtonVariant where defaultValue = ButtonVariantFilled

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
    ButtonVariantGradient _  -> "gradient"

derive instance genericVariant :: Generic ButtonVariant _
instance showVariant :: Show ButtonVariant where show = genericShow

defaultButtonProps :: ButtonProps
defaultButtonProps =
  defaultMantineComponent
    { onClick: handler_ (pure unit)
    , size: Padded Small
    }

type ButtonPropsImpl =
  MantineComponentImpl
    ( children     :: Array JSX
    , color        :: Nullable MantineColorImpl
    , disabled     :: Boolean
    , fullWidth    :: Boolean
    , gradient     :: Nullable MantineGradientImpl
    , justify      :: Nullable JustifyContentImpl
    , leftSection  :: Nullable JSX
    , loaderProps  :: Nullable LoaderPropsImpl
    , loading      :: Boolean
    , onClick      :: EventHandler
    , radius       :: Nullable RadiusImpl
    , rightSection :: Nullable JSX
    , size         :: ButtonSizeImpl
    , variant      :: ButtonVariantImpl
    )

buttonToImpl :: ButtonProps -> ButtonPropsImpl
buttonToImpl props =
  let gradient = case props.variant of
        ButtonVariantGradient g -> pure g
        _                       -> Nothing
   in toNative ({ gradient } `union` props)

buttonGroup :: (ButtonGroupProps -> ButtonGroupProps) -> JSX
buttonGroup = mkComponentWithDefault buttonGroupComponent defaultButtonGroupProps

foreign import buttonGroupComponent :: ReactComponent ButtonGroupPropsImpl

type ButtonGroupProps =
  MantineComponent
    ( borderWidth :: Maybe MantineNumberSize
    , children    :: Array JSX
    , orientation :: Orientation
    )

defaultButtonGroupProps :: ButtonGroupProps
defaultButtonGroupProps = defaultMantineComponent { orientation: Horizontal }

type ButtonGroupPropsImpl =
  MantineComponentImpl
    ( borderWidth :: Nullable MantineNumberSizeImpl
    , children    :: Array JSX
    , orientation :: OrientationImpl
    )

unstyledButton :: (UnstyledButtonProps -> UnstyledButtonProps) -> JSX
unstyledButton = mkComponentWithDefault unstyledButtonComponent defaultUnstyledButtonProps

foreign import unstyledButtonComponent :: ReactComponent UnstyledButtonPropsImpl

type UnstyledButtonProps =
  MantineComponent
    ( children :: Array JSX
    , onClick  :: EventHandler
    | Polymorphic ()
    )

defaultUnstyledButtonProps :: UnstyledButtonProps
defaultUnstyledButtonProps = defaultMantineComponent { onClick: handler_ (pure unit) }

type UnstyledButtonPropsImpl =
  MantineComponentImpl
    ( children :: Array JSX
    , onClick  :: EventHandler
    | PolymorphicImpl ()
    )
