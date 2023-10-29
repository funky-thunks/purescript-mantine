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
    , defaultOpened               :: Boolean
    , description                 :: Maybe JSX
    , disableRightSectionRotation :: Boolean
    , disabled                    :: Boolean
    , icon                        :: Maybe JSX
    , label                       :: JSX
    , noWrap                      :: Boolean
    , onChange                    :: ValueHandler Boolean
    , opened                      :: Boolean
    , rightSection                :: Maybe JSX
    , variant                     :: NavLinkVariant
    , content                     :: NavLink
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
    , icon                        :: Nullable JSX
    , label                       :: JSX
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
