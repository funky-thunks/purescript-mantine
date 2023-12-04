module Mantine.Core.Navigation.NavLink
  ( navLink
  , Props_NavLink
  , Props_NavLinkImpl
  , NavLinkVariant(..)
  , NavLinkVariantImpl
  ) where

import Mantine.Core.Prelude

navLink
  :: forall attrs attrs_ attrsImpl attrsImpl_
   . Union attrs     attrs_     Props_NavLink
  => Union attrsImpl attrsImpl_ Props_NavLinkImpl
  => ToFFI (Record attrs) (Record attrsImpl)
  => Record attrs -> JSX
navLink = element (unsafeCoerce navLinkComponent) <<< toNative

foreign import navLinkComponent :: ReactComponent (Record Props_NavLinkImpl)

type Props_NavLink =
  Props_Common
    ( active                      :: Boolean
    , children                    :: Array JSX
    , childrenOffset              :: MantineNumberSize
    , color                       :: MantineColor
    , defaultOpened               :: Boolean
    , description                 :: JSX
    , disableRightSectionRotation :: Boolean
    , disabled                    :: Boolean
    , href                        :: String
    , label                       :: JSX
    , leftSection                 :: JSX
    , noWrap                      :: Boolean
    , onChange                    :: ValueHandler Boolean
    , onClick                     :: EventHandler
    , opened                      :: Boolean
    , rightSection                :: JSX
    , variant                     :: NavLinkVariant
    )

data NavLinkVariant
  = NavLinkLight
  | NavLinkFilled
  | NavLinkSubtle

type NavLinkVariantImpl = String

instance ToFFI NavLinkVariant NavLinkVariantImpl where
  toNative = case _ of
    NavLinkLight  -> "light"
    NavLinkFilled -> "filled"
    NavLinkSubtle -> "subtle"

type Props_NavLinkImpl =
  Props_CommonImpl
    ( active                      :: Boolean
    , children                    :: Array JSX
    , childrenOffset              :: MantineNumberSizeImpl
    , color                       :: MantineColorImpl
    , defaultOpened               :: Boolean
    , description                 :: JSX
    , disableRightSectionRotation :: Boolean
    , disabled                    :: Boolean
    , href                        :: String
    , label                       :: JSX
    , leftSection                 :: JSX
    , noWrap                      :: Boolean
    , onChange                    :: ValueHandlerImpl Boolean
    , onClick                     :: EventHandler
    , opened                      :: Boolean
    , rightSection                :: JSX
    , variant                     :: NavLinkVariantImpl
    )
