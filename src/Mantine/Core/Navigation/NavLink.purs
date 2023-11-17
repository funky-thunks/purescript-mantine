module Mantine.Core.Navigation.NavLink
  ( navLink
  , NavLinkProps
  , NavLink(..)
  , NavLinkVariant(..)
  , MandatoryNavLinkProps
  ) where

import Mantine.Core.Prelude

navLink :: MandatoryNavLinkProps -> (NavLinkProps -> NavLinkProps) -> JSX
navLink = mkComponent navLinkComponent navLinkToImpl <<< defaultMantineComponent

foreign import navLinkComponent :: ReactComponent NavLinkPropsImpl

type NavLinkProps =
  MantineComponent
    ( active                      :: Boolean
    , children                    :: Array JSX
    , childrenOffset              :: Optional MantineNumberSize
    , color                       :: Optional MantineColor
    , content                     :: NavLink
    , defaultOpened               :: Boolean
    , description                 :: Optional JSX
    , disableRightSectionRotation :: Boolean
    , disabled                    :: Boolean
    , label                       :: JSX
    , leftSection                 :: Optional JSX
    , noWrap                      :: Boolean
    , onChange                    :: ValueHandler Boolean
    , opened                      :: Boolean
    , rightSection                :: Optional JSX
    , variant                     :: NavLinkVariant
    )

type MandatoryNavLinkProps =
  { label   :: JSX
  , content :: NavLink
  }

data NavLink
  = NavLink   String
  | NavButton EventHandler

type NavLinkPropsImpl =
  MantineComponentImpl
    ( active                      :: Boolean
    , children                    :: Array JSX
    , childrenOffset              :: OptionalImpl MantineNumberSizeImpl
    , color                       :: OptionalImpl MantineColorImpl
    , component                   :: String
    , defaultOpened               :: Boolean
    , description                 :: OptionalImpl JSX
    , disableRightSectionRotation :: Boolean
    , disabled                    :: Boolean
    , href                        :: OptionalImpl String
    , label                       :: JSX
    , leftSection                 :: OptionalImpl JSX
    , noWrap                      :: Boolean
    , onChange                    :: ValueHandlerImpl Boolean
    , onClick                     :: OptionalImpl EventHandler
    , opened                      :: Boolean
    , rightSection                :: OptionalImpl JSX
    , variant                     :: NavLinkVariantImpl
    )

data NavLinkVariant
  = NavLinkLight
  | NavLinkFilled
  | NavLinkSubtle

instance DefaultValue NavLinkVariant where defaultValue = NavLinkLight

type NavLinkVariantImpl = String

instance ToFFI NavLinkVariant NavLinkVariantImpl where
  toNative = case _ of
    NavLinkLight  -> "light"
    NavLinkFilled -> "filled"
    NavLinkSubtle -> "subtle"

navLinkToImpl :: NavLinkProps -> NavLinkPropsImpl
navLinkToImpl props =
  let rest = toNative <<< delete (Proxy :: Proxy "content")
      navigationProps = case props.content of
        NavLink   href    -> { component: "a",      href: pure href,        onClick: Optional Nothing }
        NavButton onClick -> { component: "button", href: Optional Nothing, onClick: pure onClick     }
   in toNative navigationProps `union` rest props
