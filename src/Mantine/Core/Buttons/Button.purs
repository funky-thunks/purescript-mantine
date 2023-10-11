module Mantine.Core.Buttons.Button
  ( button
  , button_
  , ButtonProps

  , unstyledButton
  , UnstyledButtonProps

  , buttonGroup
  , ButtonGroupProps

  , ButtonType(..)
  , LoaderPosition(..)
  , ButtonVariant(..)

  , module Mantine.Core.Common
  ) where

import Prelude hiding (bind)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Data.Show.Generic (genericShow)
import Mantine.Core.Common (MantineColor(..), MantineGradient, MantineSize(..), Orientation(..), Radius(..))
import Mantine.Core.Common as MC
import React.Basic (ReactComponent, element)
import React.Basic.Emotion as E
import React.Basic.Events (EventHandler, handler_)
import React.Basic.Hooks (JSX)

type ButtonProps =
  -- FIXME it doesn't work well with the Button component
  -- MC.ThemingProps
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
    , variant        :: Maybe ButtonVariant
    }

defaultButtonProps :: ButtonProps
defaultButtonProps =
  -- FIXME it doesn't work well with the Button component
  -- MC.defaultThemingProps
    { children: []
    , sx: E.css {}
    , color: Nothing
    , compact: false
    , disabled: false
    , fullWidth: false
    , leftIcon: Nothing
    , loaderPosition: Nothing
    , loading: false
    , onClick: handler_ (pure unit)
    , radius: Nothing
    , rightIcon: Nothing
    , size: Small
    , type: Nothing
    , uppercase: false
    , variant: Nothing
    }

type ButtonPropsImpl =
  -- FIXME it doesn't work well with the Button component
  -- MC.ThemingPropsImpl
    { children       :: Array JSX
    , sx             :: E.Style
    , color          :: Nullable String
    , compact        :: Boolean
    , disabled       :: Boolean
    , fullWidth      :: Boolean
    , gradient       :: Nullable { from :: String, to :: String, angle :: Nullable Number }
    , leftIcon       :: Nullable JSX
    , loaderPosition :: Nullable String
    , loading        :: Boolean
    , onClick        :: EventHandler
    , radius         :: Nullable String
    , rightIcon      :: Nullable JSX
    , size           :: String
    , type           :: Nullable String
    , uppercase      :: Boolean
    , variant        :: Nullable String
    }

type ButtonGroupProps =
  -- FIXME it doesn't work well with the Button component
  -- MC.ThemingProps
    { children    :: Array JSX
    , orientation :: Orientation
    }

defaultButtonGroupProps :: ButtonGroupProps
defaultButtonGroupProps =
  -- FIXME it doesn't work well with the Button component
  -- MC.defaultThemingProps
    { orientation: Horizontal
    , children: []
    }

type ButtonGroupPropsImpl =
  -- FIXME it doesn't work well with the Button component
  -- MC.ThemingPropsImpl
    { children    :: Array JSX
    , orientation :: String
    }

data ButtonType
  = Button
  | Reset
  | Submit

data LoaderPosition
  = LoaderPositionLeft
  | LoaderPositionRight
  | LoaderPositionCenter

data ButtonVariant
  = ButtonVariantDefault
  | ButtonVariantFilled
  | ButtonVariantOutline
  | ButtonVariantSubtle
  | ButtonVariantLight
  | ButtonVariantWhite
  | ButtonVariantGradient MantineGradient

derive instance genericVariant :: Generic ButtonVariant _
instance showVariant :: Show ButtonVariant where show = genericShow

buttonToImpl :: ButtonProps -> ButtonPropsImpl
buttonToImpl =
  -- FIXME it doesn't work well with the Button component
  {- MC.themingToImpl -} \ props@{ children, sx, loading, compact, fullWidth, disabled, uppercase, onClick } ->
    let
        loaderPositionNative = case _ of
          LoaderPositionLeft -> "left"
          LoaderPositionRight -> "right"
          LoaderPositionCenter -> "center"

        variantNative = case _ of
          ButtonVariantOutline    -> "outline"
          ButtonVariantWhite      -> "white"
          ButtonVariantLight      -> "light"
          ButtonVariantDefault    -> "default"
          ButtonVariantFilled     -> "filled"
          ButtonVariantSubtle     -> "subtle"
          ButtonVariantGradient _ -> "gradient"

        gradient = case _ of
          ButtonVariantGradient g -> pure (MC.gradientNative g)
          _                       -> Nothing

        buttonNative = case _ of
          Button -> "button"
          Reset  -> "reset"
          Submit -> "submit"

        radiusNative = case _ of
          Radius       nr -> show nr
          RadiusPreset s  -> MC.sizeNative s

     in
        { children, sx, loading, compact, fullWidth, disabled, uppercase, onClick
        , size:           MC.sizeNative props.size
        , leftIcon:       toNullable props.leftIcon
        , rightIcon:      toNullable props.rightIcon
        , gradient:       toNullable $ gradient =<< props.variant
        , loaderPosition: toNullable $ loaderPositionNative <$> props.loaderPosition
        , variant:        toNullable $ variantNative        <$> props.variant
        , type:           toNullable $ buttonNative         <$> props.type
        , radius:         toNullable $ radiusNative         <$> props.radius
        , color:          toNullable $ MC.colorNative       <$> props.color
        }

buttonGroupToImpl :: ButtonGroupProps -> ButtonGroupPropsImpl
buttonGroupToImpl =
  -- FIXME it doesn't work well with the Button component
  {- MC.themingToImpl -} \ props@{ children } ->
    let orientationNative =
          case _ of
            Horizontal -> "horizontal"
            Vertical   -> "vertical"
     in { children
        , orientation: orientationNative props.orientation
        }

button :: (ButtonProps -> ButtonProps) -> JSX
button setProps = element buttonComponent (buttonToImpl (setProps defaultButtonProps))

button_ :: JSX -> JSX
button_ child = button _ { children = pure child }

foreign import buttonComponent :: ReactComponent ButtonPropsImpl

buttonGroup :: (ButtonGroupProps -> ButtonGroupProps) -> JSX
buttonGroup setProps = element buttonGroupComponent (buttonGroupToImpl (setProps defaultButtonGroupProps))

foreign import buttonGroupComponent :: ReactComponent ButtonGroupPropsImpl

unstyledButton :: (UnstyledButtonProps -> UnstyledButtonProps) -> JSX
unstyledButton setProps = element unstyledButtonComponent (setProps defaultUnstyledButtonProps)

defaultUnstyledButtonProps :: UnstyledButtonProps
defaultUnstyledButtonProps =
  MC.defaultThemingProps
    { children: []
    , onClick: handler_ (pure unit)
    }

type UnstyledButtonProps =
  MC.ThemingProps
    ( children :: Array JSX
    , onClick  :: EventHandler
    )

foreign import unstyledButtonComponent :: ReactComponent UnstyledButtonProps
