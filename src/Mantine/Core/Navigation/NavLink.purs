module Mantine.Core.Navigation.NavLink
  ( navLink
  , NavLinkProps
  , NavLinkVariant(..)
  ) where

import Prelude
import Data.Default (defaultValue)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Effect.Uncurried (EffectFn1)
import Mantine.Core.Common (MantineColor, MantineNumberSize, ValueHandler)
import Mantine.Core.Common as MC
import Mantine.FFI (class ToFFI, toNative)
import React.Basic (ReactComponent, element)
import React.Basic.Events (EventHandler, handler_)
import React.Basic.Hooks (JSX)
import Record (union)

navLink :: (NavLinkProps -> NavLinkProps) -> JSX
navLink setProps = element navLinkComponent (navLinkToImpl (setProps defaultNavLinkProps))

foreign import navLinkComponent :: ReactComponent NavLinkPropsImpl

type NavLinkProps =
  MC.ThemingProps
    ( active                      :: Boolean
    , children                    :: Array JSX
    , childrenOffset              :: Maybe MantineNumberSize
    , color                       :: Maybe MantineColor
    , defaultOpened               :: Boolean
    , description                 :: Maybe JSX
    , disableRightSectionRotation :: Boolean
    , disabled                    :: Boolean
    , href                        :: Maybe String
    , icon                        :: Maybe JSX
    , label                       :: JSX
    , noWrap                      :: Boolean
    , onChange                    :: ValueHandler Boolean
    , onClick                     :: EventHandler
    , opened                      :: Boolean
    , rightSection                :: Maybe JSX
    , variant                     :: NavLinkVariant
    )

defaultNavLinkProps :: NavLinkProps
defaultNavLinkProps =
  MC.defaultThemingProps
    { label:   mempty :: JSX
    , onClick: handler_ (pure unit)
    , variant: NavLinkLight
    } `union` defaultValue

type NavLinkPropsImpl =
  MC.ThemingPropsImpl
    ( active                      :: Boolean
    , children                    :: Array JSX
    , childrenOffset              :: Nullable MC.MantineNumberSizeImpl
    , color                       :: Nullable String
    , component                   :: Nullable String
    , defaultOpened               :: Boolean
    , description                 :: Nullable JSX
    , disableRightSectionRotation :: Boolean
    , disabled                    :: Boolean
    , href                        :: Nullable String
    , icon                        :: Nullable JSX
    , label                       :: JSX
    , noWrap                      :: Boolean
    , onChange                    :: EffectFn1 Boolean Unit
    , onClick                     :: EventHandler
    , opened                      :: Boolean
    , rightSection                :: Nullable JSX
    , variant                     :: String
    )

data NavLinkVariant
  = NavLinkLight
  | NavLinkFilled
  | NavLinkSubtle

instance ToFFI NavLinkVariant String where
  toNative = case _ of
    NavLinkLight  -> "light"
    NavLinkFilled -> "filled"
    NavLinkSubtle -> "subtle"

navLinkToImpl :: NavLinkProps -> NavLinkPropsImpl
navLinkToImpl props =
  toNative props `union` toNative { component: "a" <$ props.href }
