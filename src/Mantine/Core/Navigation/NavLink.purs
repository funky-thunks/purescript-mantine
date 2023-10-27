module Mantine.Core.Navigation.NavLink
  ( navLink
  , NavLinkProps
  , NavLinkVariant(..)
  ) where

import Mantine.Core.Prelude

navLink :: (NavLinkProps -> NavLinkProps) -> JSX
navLink = mkComponent navLinkComponent navLinkToImpl defaultNavLinkProps

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
  defaultThemingProps
    { label:   mempty :: JSX
    , onClick: handler_ (pure unit)
    }

type NavLinkPropsImpl =
  ThemingPropsImpl
    ( active                      :: Boolean
    , children                    :: Array JSX
    , childrenOffset              :: Nullable MantineNumberSizeImpl
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

instance DefaultValue NavLinkVariant where defaultValue = NavLinkLight

instance ToFFI NavLinkVariant String where
  toNative = case _ of
    NavLinkLight  -> "light"
    NavLinkFilled -> "filled"
    NavLinkSubtle -> "subtle"

navLinkToImpl :: NavLinkProps -> NavLinkPropsImpl
navLinkToImpl props =
  toNative props `union` toNative { component: "a" <$ props.href }
