module Mantine.Core.Navigation.NavLink
  ( navLink
  , NavLinkProps
  , NavLink(..)
  , NavLinkVariant(..)
  , MandatoryNavLinkProps
  ) where

import Mantine.Core.Prelude

navLink :: MandatoryNavLinkProps -> (NavLinkProps -> NavLinkProps) -> JSX
navLink = mkComponent navLinkComponent navLinkToImpl <<< defaultThemingProps

foreign import navLinkComponent :: ReactComponent NavLinkPropsImpl

type NavLinkProps =
  ThemingProps
    ( active                      :: Boolean
    , children                    :: Array JSX
    , childrenOffset              :: Maybe MantineNumberSize
    , color                       :: Maybe MantineColor
    , content                     :: NavLink
    , defaultOpened               :: Boolean
    , description                 :: Maybe JSX
    , disableRightSectionRotation :: Boolean
    , disabled                    :: Boolean
    , label                       :: JSX
    , leftSection                 :: Maybe JSX
    , noWrap                      :: Boolean
    , onChange                    :: ValueHandler Boolean
    , opened                      :: Boolean
    , rightSection                :: Maybe JSX
    , variant                     :: NavLinkVariant
    )

type MandatoryNavLinkProps =
  { label   :: JSX
  , content :: NavLink
  }

data NavLink = NavLink   String
             | NavButton EventHandler

type NavLinkPropsImpl =
  ThemingPropsImpl
    ( active                      :: Boolean
    , children                    :: Array JSX
    , childrenOffset              :: Nullable MantineNumberSizeImpl
    , color                       :: Nullable String
    , component                   :: String
    , defaultOpened               :: Boolean
    , description                 :: Nullable JSX
    , disableRightSectionRotation :: Boolean
    , disabled                    :: Boolean
    , href                        :: Nullable String
    , label                       :: JSX
    , leftSection                 :: Nullable JSX
    , noWrap                      :: Boolean
    , onChange                    :: EffectFn1 Boolean Unit
    , onClick                     :: Nullable EventHandler
    , opened                      :: Boolean
    , rightSection                :: Nullable JSX
    , variant                     :: String
    )

data NavLinkVariant
  = NavLinkLight
  | NavLinkFilled
  | NavLinkSubtle

instance DefaultValue NavLinkVariant where defaultValue = NavLinkLight

instance ToFFI NavLinkVariant String where
  toNative = case _ of
    NavLinkLight  -> "light"
    NavLinkFilled -> "filled"
    NavLinkSubtle -> "subtle"

navLinkToImpl :: NavLinkProps -> NavLinkPropsImpl
navLinkToImpl props =
  let rest = toNative <<< delete (Proxy :: Proxy "content")
      navigationProps = case props.content of
        NavLink   href    -> { component: "a",      href: Just href, onClick: Nothing      }
        NavButton onClick -> { component: "button", href: Nothing  , onClick: Just onClick }
   in toNative navigationProps `union` rest props
