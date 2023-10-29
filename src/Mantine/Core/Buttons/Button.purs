module Mantine.Core.Buttons.Button
  ( button
  , button_
  , ButtonProps
  , ButtonType(..)
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
import Mantine.Core.Prelude
import React.Basic.Emotion as E
import React.Basic.Events (EventHandler, handler_)

button :: (ButtonProps -> ButtonProps) -> JSX
button = mkComponent buttonComponent buttonToImpl defaultButtonProps

button_ :: JSX -> JSX
button_ child = button _ { children = pure child }

foreign import buttonComponent :: ReactComponent ButtonPropsImpl

type ButtonProps =
  -- FIXME it doesn't work well with the Button component
  -- ThemingProps
    { children       :: Array JSX
    , sx             :: E.Style
    , color          :: Maybe MantineColor
    , compact        :: Boolean
    , disabled       :: Boolean
    , fullWidth      :: Boolean
    , leftIcon       :: Maybe JSX
    , loaderPosition :: Maybe LoaderPosition
    , loading        :: Boolean
    , onClick        :: EventHandler
    , radius         :: Maybe Radius
    , rightIcon      :: Maybe JSX
    , size           :: MantineSize
    , type           :: Maybe ButtonType
    , uppercase      :: Boolean
    , variant        :: ButtonVariant
    }

data ButtonType
  = Button
  | Reset
  | Submit

instance ToFFI ButtonType String where
  toNative = case _ of
    Button -> "button"
    Reset  -> "reset"
    Submit -> "submit"

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
  | ButtonVariantOutline
  | ButtonVariantSubtle
  | ButtonVariantLight
  | ButtonVariantWhite
  | ButtonVariantGradient MantineGradient

instance DefaultValue ButtonVariant where defaultValue = ButtonVariantFilled

instance ToFFI ButtonVariant String where
  toNative = case _ of
    ButtonVariantOutline    -> "outline"
    ButtonVariantWhite      -> "white"
    ButtonVariantLight      -> "light"
    ButtonVariantDefault    -> "default"
    ButtonVariantFilled     -> "filled"
    ButtonVariantSubtle     -> "subtle"
    ButtonVariantGradient _ -> "gradient"

derive instance genericVariant :: Generic ButtonVariant _
instance showVariant :: Show ButtonVariant where show = genericShow

defaultButtonProps :: ButtonProps
defaultButtonProps =
  -- FIXME it doesn't work well with the Button component
  -- defaultThemingProps
    { sx: E.css {}
    , onClick: handler_ (pure unit)
    , size: Small
    } `union` defaultValue

type ButtonPropsImpl =
  -- FIXME it doesn't work well with the Button component
  -- ThemingPropsImpl
    { children       :: Array JSX
    , sx             :: E.Style
    , color          :: Nullable String
    , compact        :: Boolean
    , disabled       :: Boolean
    , fullWidth      :: Boolean
    , gradient       :: Nullable MantineGradientImpl
    , leftIcon       :: Nullable JSX
    , loaderPosition :: Nullable String
    , loading        :: Boolean
    , onClick        :: EventHandler
    , radius         :: Nullable String
    , rightIcon      :: Nullable JSX
    , size           :: String
    , type           :: Nullable String
    , uppercase      :: Boolean
    , variant        :: String
    }

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
  -- FIXME it doesn't work well with the Button component
  -- ThemingProps
    { children    :: Array JSX
    , orientation :: Orientation
    }

defaultButtonGroupProps :: ButtonGroupProps
defaultButtonGroupProps =
  -- FIXME it doesn't work well with the Button component
  -- defaultThemingProps
    { orientation: Horizontal
    } `union` defaultValue

type ButtonGroupPropsImpl =
  -- FIXME it doesn't work well with the Button component
  -- ThemingPropsImpl
    { children    :: Array JSX
    , orientation :: String
    }

unstyledButton :: (UnstyledButtonProps -> UnstyledButtonProps) -> JSX
unstyledButton = mkComponentWithDefault unstyledButtonComponent defaultUnstyledButtonProps

foreign import unstyledButtonComponent :: ReactComponent UnstyledButtonPropsImpl

type UnstyledButtonProps =
  ThemingProps
    ( children :: Array JSX
    , onClick  :: EventHandler
    )

defaultUnstyledButtonProps :: UnstyledButtonProps
defaultUnstyledButtonProps = defaultThemingProps { onClick: handler_ (pure unit) }

type UnstyledButtonPropsImpl =
  ThemingPropsImpl
    ( children :: Array JSX
    , onClick  :: EventHandler
    )
