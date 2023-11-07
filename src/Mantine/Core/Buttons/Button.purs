module Mantine.Core.Buttons.Button
  ( button
  , button_
  , ButtonProps
  , ButtonVariant(..)
  , LoaderPosition(..)

  , buttonGroup
  , ButtonGroupProps

  , unstyledButton
  , UnstyledButtonProps
  ) where

import Prelude (class Show)
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
  ThemingProps
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
    , size         :: MantineSize
    , variant      :: ButtonVariant
    )

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

instance ToFFI ButtonVariant String where
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
  defaultThemingProps
    { onClick: handler_ (pure unit)
    , size: Small
    }

type ButtonPropsImpl =
  ThemingPropsImpl
    ( children       :: Array JSX
    , color          :: Nullable String
    , disabled       :: Boolean
    , fullWidth      :: Boolean
    , gradient       :: Nullable MantineGradientImpl
    , justify        :: Nullable String
    , leftSection    :: Nullable JSX
    , loaderProps    :: Nullable LoaderPropsImpl
    , loading        :: Boolean
    , onClick        :: EventHandler
    , radius         :: Nullable String
    , rightSection   :: Nullable JSX
    , size           :: String
    , variant        :: String
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
  ThemingProps
    ( borderWidth :: Maybe MantineNumberSize
    , children    :: Array JSX
    , orientation :: Orientation
    )

defaultButtonGroupProps :: ButtonGroupProps
defaultButtonGroupProps = defaultThemingProps { orientation: Horizontal }

type ButtonGroupPropsImpl =
  ThemingPropsImpl
    ( borderWidth :: Nullable MantineNumberSizeImpl
    , children    :: Array JSX
    , orientation :: String
    )

unstyledButton :: (UnstyledButtonProps -> UnstyledButtonProps) -> JSX
unstyledButton = mkComponentWithDefault unstyledButtonComponent defaultUnstyledButtonProps

foreign import unstyledButtonComponent :: ReactComponent UnstyledButtonPropsImpl

type UnstyledButtonProps =
  ThemingProps
    ( children :: Array JSX
    , onClick  :: EventHandler
    | Polymorphic ()
    )

defaultUnstyledButtonProps :: UnstyledButtonProps
defaultUnstyledButtonProps = defaultThemingProps { onClick: handler_ (pure unit) }

type UnstyledButtonPropsImpl =
  ThemingPropsImpl
    ( children :: Array JSX
    , onClick  :: EventHandler
    | PolymorphicImpl ()
    )
