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
  ) where

import Mantine.Core.Prelude

appShell :: (AppShellProps -> AppShellProps) -> JSX
appShell = mkComponent appShellComponent appShellToImpl defaultThemingProps_

foreign import appShellComponent :: ReactComponent AppShellPropsImpl

type AppShellProps =
  ThemingProps
    ( alternativeLayout      :: Boolean
    , aside                  :: Maybe JSX
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
  ThemingPropsImpl
    ( aside                  :: Nullable JSX
    , asideOffsetBreakpoint  :: Nullable MantineNumberSizeImpl
    , children               :: Array JSX
    , fixed                  :: Boolean
    , footer                 :: Nullable JSX
    , header                 :: Nullable JSX
    , hidden                 :: Boolean
    , layout                 :: String
    , navbar                 :: Nullable JSX
    , navbarOffsetBreakpoint :: Nullable MantineNumberSizeImpl
    , padding                :: Nullable MantineNumberSizeImpl
    , zIndex                 :: Nullable Number
    )

appShellToImpl :: AppShellProps -> AppShellPropsImpl
appShellToImpl props =
  let rest = toNative <<< delete (Proxy :: Proxy "alternativeLayout")
   in { layout: if props.alternativeLayout then "alt" else "default" } `union` rest props

navbar :: (NavbarProps -> NavbarProps) -> JSX
navbar = mkTrivialComponent navbarComponent

navbar_ :: Array JSX -> JSX
navbar_ children = navbar _ { children = children }

aside :: (AsideProps -> AsideProps) -> JSX
aside = mkTrivialComponent asideComponent

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
  ThemingProps
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
  ThemingPropsImpl
    ( children         :: Array JSX
    , fixed            :: Boolean
    , height           :: Nullable MantineNumberSizeImpl
    , hidden           :: Boolean
    , hiddenBreakpoint :: Nullable MantineNumberSizeImpl
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
navbarSection = mkTrivialComponent navbarSectionComponent

navbarSection_ :: Array JSX -> JSX
navbarSection_ children = navbarSection _ { children = children }

foreign import navbarSectionComponent :: ReactComponent NavbarSectionPropsImpl

type NavbarSectionProps =
  ThemingProps
    ( children :: Array JSX
    , grow     :: Boolean
    )

type NavbarSectionPropsImpl =
  ThemingPropsImpl
    ( children :: Array JSX
    , grow     :: Boolean
    )

header :: (HeaderProps -> HeaderProps) -> JSX
header = mkTrivialComponent headerComponent

footer :: (FooterProps -> FooterProps) -> JSX
footer = mkTrivialComponent footerComponent

foreign import headerComponent :: ReactComponent VerticalSectionPropsImpl
foreign import footerComponent :: ReactComponent VerticalSectionPropsImpl

type HeaderProps = VerticalSectionProps
type FooterProps = VerticalSectionProps

type VerticalSectionProps =
  ThemingProps
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
  ThemingPropsImpl
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
