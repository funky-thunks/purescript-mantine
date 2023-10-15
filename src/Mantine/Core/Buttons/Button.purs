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
import Data.Default (defaultValue)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Data.Show.Generic (genericShow)
import Mantine.Core.Common (MantineColor(..), MantineGradient, MantineSize(..), Orientation(..), Radius(..))
import Mantine.Core.Common as MC
import Mantine.FFI (class ToFFI, toNative)
import React.Basic (ReactComponent, element)
import React.Basic.Emotion as E
import React.Basic.Events (EventHandler, handler_)
import React.Basic.Hooks (JSX)
import Record (union)

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
    { sx: E.css {}
    , onClick: handler_ (pure unit)
    , size: Small
    } `union` defaultValue

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
    } `union` defaultValue

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

buttonToImpl :: ButtonProps -> ButtonPropsImpl
buttonToImpl props =
  let gradient = case _ of
        ButtonVariantGradient g -> pure (toNative g)
        _                       -> Nothing

   in { gradient: toNullable $ gradient =<< props.variant
      } `union` toNative props

button :: (ButtonProps -> ButtonProps) -> JSX
button setProps = element buttonComponent (buttonToImpl (setProps defaultButtonProps))

button_ :: JSX -> JSX
button_ child = button _ { children = pure child }

foreign import buttonComponent :: ReactComponent ButtonPropsImpl

buttonGroup :: (ButtonGroupProps -> ButtonGroupProps) -> JSX
buttonGroup setProps = element buttonGroupComponent (toNative (setProps defaultButtonGroupProps))

foreign import buttonGroupComponent :: ReactComponent ButtonGroupPropsImpl

unstyledButton :: (UnstyledButtonProps -> UnstyledButtonProps) -> JSX
unstyledButton setProps = element unstyledButtonComponent (setProps defaultUnstyledButtonProps)

defaultUnstyledButtonProps :: UnstyledButtonProps
defaultUnstyledButtonProps =
  MC.defaultThemingProps
    { onClick: handler_ (pure unit)
    } `union` defaultValue

type UnstyledButtonProps =
  MC.ThemingProps
    ( children :: Array JSX
    , onClick  :: EventHandler
    )

foreign import unstyledButtonComponent :: ReactComponent UnstyledButtonProps
