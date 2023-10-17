module Mantine.Core.Layout.AppShell
  ( appShell
  , AppShellProps

  , navbar
  , navbar_
  , aside
  , aside_
  , HorizontalSectionProps
  , HorizontalSectionHeight(..)
  , HorizontalSectionPosition
  , navbarSection
  , navbarSection_
  , NavbarSectionProps

  , header
  , footer
  , VerticalSectionProps
  , VerticalSectionHeight(..)
  , VerticalSectionPosition
  , Rules(..)

  , module Mantine.Core.Common
  ) where

import Prelude (map, (<$>))
import Data.Default (class DefaultValue)
import Data.Either (Either, either)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Tuple.Nested (type (/\))
import Foreign.Object (Object, fromFoldable)
import Mantine.Core.Common (MantineSize(..), MantineNumberSize)
import Mantine.Core.Common as MC
import Mantine.FFI (class ToFFI, toNative)
import React.Basic (ReactComponent, element)
import React.Basic.Hooks (JSX)
import Untagged.Union (type (|+|), asOneOf)

appShell :: (AppShellProps -> AppShellProps) -> JSX
appShell setProps = element appShellComponent (toNative (setProps MC.defaultThemingProps_))

foreign import appShellComponent :: ReactComponent AppShellPropsImpl

type AppShellProps =
  MC.ThemingProps
    ( aside                  :: Maybe JSX
    , asideOffsetBreakpoint  :: Maybe MantineNumberSize
    , children               :: Array JSX
    , fixed                  :: Boolean
    , footer                 :: Maybe JSX
    , header                 :: Maybe JSX
    , hidden                 :: Boolean
    , navbar                 :: Maybe JSX
    , navbarOffsetBreakpoint :: Maybe MantineNumberSize
    , padding                :: Maybe MantineNumberSize
    , zIndex                 :: Maybe Number
    )

type AppShellPropsImpl =
  MC.ThemingPropsImpl
    ( aside                  :: Nullable JSX
    , asideOffsetBreakpoint  :: Nullable MC.MantineNumberSizeImpl
    , children               :: Array JSX
    , fixed                  :: Boolean
    , footer                 :: Nullable JSX
    , header                 :: Nullable JSX
    , hidden                 :: Boolean
    , navbar                 :: Nullable JSX
    , navbarOffsetBreakpoint :: Nullable MC.MantineNumberSizeImpl
    , padding                :: Nullable MC.MantineNumberSizeImpl
    , zIndex                 :: Nullable Number
    )

navbar :: (NavbarProps -> NavbarProps) -> JSX
navbar setProps = element navbarComponent (toNative (setProps MC.defaultThemingProps_))

navbar_ :: Array JSX -> JSX
navbar_ children = navbar _ { children = children }

aside :: (AsideProps -> AsideProps) -> JSX
aside setProps = element asideComponent (toNative (setProps MC.defaultThemingProps_))

aside_ :: Array JSX -> JSX
aside_ children = aside _ { children = children }

foreign import navbarComponent :: ReactComponent HorizontalSectionPropsImpl
foreign import asideComponent  :: ReactComponent HorizontalSectionPropsImpl

type NavbarProps = HorizontalSectionProps
type AsideProps = HorizontalSectionProps

data HorizontalSectionHeight
  = HorizontalSectionHeightNumeric Number
  | HorizontalSectionHeightString  String

instance ToFFI HorizontalSectionHeight (Number |+| String) where
  toNative = case _ of
    HorizontalSectionHeightNumeric nr -> asOneOf nr
    HorizontalSectionHeightString  s  -> asOneOf s

type HorizontalSectionProps =
  MC.ThemingProps
    ( children         :: Array JSX
    , fixed            :: Boolean
    , height           :: Maybe HorizontalSectionHeight
    , hidden           :: Boolean
    , hiddenBreakpoint :: Maybe MantineNumberSize
    , position         :: Maybe HorizontalSectionPosition
    , width            :: Rules
    , withBorder       :: Boolean
    , zIndex           :: Maybe Number
    )

type HorizontalSectionPosition =
  { left :: Maybe Number
  , top  :: Maybe Number
  }

newtype Rules = Rules (Array (String /\ Either String Number))

instance DefaultValue Rules where defaultValue = Rules []

instance ToFFI Rules (Object (String |+| Number)) where
  toNative (Rules rs) =
    let f = either asOneOf asOneOf
     in fromFoldable (map f <$> rs)

type HorizontalSectionPropsImpl =
  MC.ThemingPropsImpl
    ( children         :: Array JSX
    , fixed            :: Boolean
    , height           :: Nullable MC.MantineNumberSizeImpl
    , hidden           :: Boolean
    , hiddenBreakpoint :: Nullable MC.MantineNumberSizeImpl
    , position         :: Nullable HorizontalSectionPositionImpl
    , width            :: Object (String |+| Number)
    , withBorder       :: Boolean
    , zIndex           :: Nullable Number
    )

type HorizontalSectionPositionImpl =
  { left :: Nullable Number
  , top  :: Nullable Number
  }

navbarSection :: (NavbarSectionProps -> NavbarSectionProps) -> JSX
navbarSection setProps = element navbarSectionComponent (toNative (setProps MC.defaultThemingProps_))

navbarSection_ :: Array JSX -> JSX
navbarSection_ children = navbarSection _ { children = children }

foreign import navbarSectionComponent :: ReactComponent NavbarSectionPropsImpl

type NavbarSectionProps =
  MC.ThemingProps
    ( children :: Array JSX
    , grow     :: Boolean
    )

type NavbarSectionPropsImpl =
  MC.ThemingPropsImpl
    ( children :: Array JSX
    , grow     :: Boolean
    )

header :: (HeaderProps -> HeaderProps) -> JSX
header setProps = element headerComponent (toNative (setProps MC.defaultThemingProps_))

footer :: (FooterProps -> FooterProps) -> JSX
footer setProps = element footerComponent (toNative (setProps MC.defaultThemingProps_))

foreign import headerComponent :: ReactComponent VerticalSectionPropsImpl
foreign import footerComponent :: ReactComponent VerticalSectionPropsImpl

type HeaderProps = VerticalSectionProps
type FooterProps = VerticalSectionProps

type VerticalSectionProps =
  MC.ThemingProps
    ( children   :: Array JSX
    , fixed      :: Boolean
    , height     :: Maybe VerticalSectionHeight
    , position   :: Maybe VerticalSectionPosition
    , withBorder :: Boolean
    , zIndex     :: Maybe Number
    )

data VerticalSectionHeight
  = VerticalSectionHeightNumeric Number
  | VerticalSectionHeightString String
  | VerticalSectionHeightRules Rules

-- number | string | Partial<Record<string, string | number>>
type VerticalSectionHeightImpl = Number |+| String |+| Object (String |+| Number)

instance ToFFI VerticalSectionHeight VerticalSectionHeightImpl where
  toNative = case _ of
    VerticalSectionHeightNumeric nr -> asOneOf nr
    VerticalSectionHeightString  s  -> asOneOf s
    VerticalSectionHeightRules   rs -> asOneOf (toNative rs)

type VerticalSectionPosition =
  { bottom :: Maybe Number
  , left   :: Maybe Number
  , right  :: Maybe Number
  , top    :: Maybe Number
  }

type VerticalSectionPropsImpl =
  MC.ThemingPropsImpl
    ( children   :: Array JSX
    , fixed      :: Boolean
    , height     :: Nullable VerticalSectionHeightImpl
    , position   :: Nullable VerticalSectionPositionImpl
    , withBorder :: Boolean
    , zIndex     :: Nullable Number
    )

type VerticalSectionPositionImpl =
  { bottom :: Nullable Number
  , left   :: Nullable Number
  , right  :: Nullable Number
  , top    :: Nullable Number
  }
